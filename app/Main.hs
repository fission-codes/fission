module Main (main) where

import RIO

import Control.Lens ((.~))
import Data.Aeson (decodeFileStrict)
import System.Envy

import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Network.Wai.Middleware.RequestLogger

import Fission
import Fission.Types
import Fission.Storage.SQLite as SQLite

import           Fission.Internal.Orphanage ()
import qualified Fission.Internal.UTF8 as UTF8

import           Fission.Environment
import qualified Fission.Monitor       as Monitor
import qualified Fission.Web           as Web
import qualified Fission.Web.Types     as Web
import           Fission.Storage.Types as DB

import qualified Fission.Platform.Heroku.AddOn.Manifest as Manifest
import           Fission.Platform.Heroku.AddOn.Manifest hiding (id)

main :: IO ()
main = withStdoutLogger $ \stdOut -> do
  Just (manifest :: Manifest) <- decodeFileStrict "./addon-manifest.json"

  Web.Port port <- getEnv
  DB.Pool pool  <- simply setupPool

  hostURL       <- withEnv "HOST" "localhost:3000" UTF8.textShow
  condDebug     <- withFlag "DEBUG_REQS" id logStdoutDev

  runRIO (mkConfig' manifest pool hostURL) do
    condMonitor
    logInfo $ "Servant running at port " <> display port
    liftIO . runSettings (mkSettings stdOut port) . condDebug =<< Web.app =<< ask

mkConfig' :: Manifest -> SeldaPool -> Text -> Config
mkConfig' manifest pool url = cfg & host .~ Web.Host url
  where
    cfg   = mkConfig hID hPass (DB.Pool pool)
    hID   = HerokuID       . encodeUtf8 $ manifest ^. Manifest.id
    hPass = HerokuPassword . encodeUtf8 $ manifest ^. api ^. password

mkSettings :: ApacheLogger -> Port -> Settings
mkSettings stdOut port = portSettings $ logSettings defaultSettings
  where
    portSettings = setPort port
    logSettings  = setLogger stdOut

condMonitor :: HasLogFunc cfg => RIO cfg ()
condMonitor = do
  monitorFlag <- liftIO $ getFlag "MONITOR"
  when monitorFlag Monitor.wai

setupPool :: HasLogFunc cfg => RIO cfg DB.Pool
setupPool = liftIO decode
        >>= return . maybe (DB.Path "ipfs-api.sqlite") id
        >>= SQLite.connPool
