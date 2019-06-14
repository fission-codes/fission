module Main (main) where

import RIO
import RIO.Char (toLower)

import Control.Lens ((.~))
import Data.Aeson (decodeFileStrict)
import System.Environment

import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Network.Wai.Middleware.RequestLogger

import Fission.Config         as Config
import Fission.Storage.SQLite as SQLite

import           Fission.Internal.Orphanage ()
import qualified Fission.Internal.UTF8 as UTF8

import qualified Fission.Log                            as Log
import qualified Fission.Monitor                        as Monitor
import qualified Fission.Web                            as Web
import qualified Fission.Web.Config                     as Web.Config

import qualified Fission.Platform.Heroku.AddOn.Manifest as Manifest
import           Fission.Platform.Heroku.AddOn.Manifest hiding (id)

main :: IO ()
main = withStdoutLogger $ \stdOut -> do
  Just (manifest :: Manifest) <- decodeFileStrict "./addon-manifest.json"
  pool      <- simply setupPool
  hostURL   <- withEnv "HOST" "localhost:3000" UTF8.textShow
  condDebug <- withFlag "DEBUG_REQS" id logStdoutDev

  runRIO (mkConfig manifest pool hostURL) do
    condMonitor
    Web.Config.Config port <- Web.Config.get
    logInfo $ "Servant running at port " <> display port

    liftIO . runSettings (mkSettings stdOut port) . condDebug =<< Web.app =<< ask

simply :: RIO LogFunc a -> IO a
simply = runRIO (mkLogFunc Log.simple)

mkConfig :: Manifest -> SeldaPool -> Text -> Config
mkConfig manifest pool url = cfg & host .~ Host url
  where
    cfg   = Config.base hID hPass (DBPool pool)
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

withFlag :: forall a. String -> a -> a -> IO a
withFlag key whenFalse whenTrue = withEnv key whenFalse (const whenTrue)

withEnv :: String -> a -> (String -> a) -> IO a
withEnv key fallback transform = pure . maybe fallback transform =<< lookupEnv key

getFlag :: String -> IO Bool
getFlag key =
  pure . maybe False (\flag -> fmap toLower flag == "true") =<< lookupEnv key

setupPool :: HasLogFunc cfg => RIO cfg SeldaPool
setupPool = do
  dbPathEnv <- liftIO $ lookupEnv "DB_PATH"
  dbPath'   <- return $ maybe "ipfs-api.sqlite" DBPath dbPathEnv
  SQLite.connPool dbPath'
