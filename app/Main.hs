module Main (main) where

import RIO
import RIO.Char (toLower)

import Control.Lens ((.~))
import Data.Aeson (decodeFileStrict)
import System.Environment

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Network.Wai.Middleware.RequestLogger

import Fission.Config         as Config
import Fission.Storage.SQLite as SQLite

import Fission.Internal.Orphanage ()

import qualified Fission.Log                            as Log
import qualified Fission.Monitor                        as Monitor
import qualified Fission.Web                            as Web
import qualified Fission.Web.Config                     as Web.Config

import qualified Fission.Platform.Heroku.AddOn.Manifest as Manifest
import           Fission.Platform.Heroku.AddOn.Manifest hiding (id)

main :: IO ()
main = withStdoutLogger $ \stdOut -> do
  Just (manifest :: Manifest) <- decodeFileStrict "./addon-manifest.json"
  pool <- simply setupPool
  runRIO (mkConfig manifest pool) do
    condMonitor
    Web.Config.Config port <- Web.Config.get
    logInfo $ "Servant running at port " <> display port

    let middleware = runSettings (mkSettings stdOut port) <=< condDebugReqs
    liftIO . middleware $ Web.app =<< ask

simply :: RIO LogFunc a -> IO a
simply = runRIO (mkLogFunc Log.simple)

condDebugReqs :: IO (Application -> Application)
condDebugReqs = do
  debugReqsEnv <- liftIO $ lookupEnv "DEBUG_REQS"
  return $ maybe id logOrNot debugReqsEnv
  where
    logOrNot :: String -> (Application -> Application)
    logOrNot str =
      if fmap toLower str == "true"
         then logStdoutDev
         else id

condMonitor :: HasLogFunc cfg => RIO cfg ()
condMonitor = do
  monitorEnv <- liftIO $ lookupEnv "MONITOR"
  monitored  <- return $ maybe False ((== "true") . fmap toLower) monitorEnv
  when monitored Monitor.wai

mkSettings :: ApacheLogger -> Port -> Settings
mkSettings stdOut port = portSettings $ logSettings defaultSettings
  where
    portSettings = setPort port
    logSettings  = setLogger stdOut

mkConfig :: Manifest -> SeldaPool -> Maybe String -> Config
mkConfig manifest pool = \case
  Nothing   -> cfg
  Just path -> cfg & host .~ Host (textDisplay path)
  where
    cfg   = Config.base hID hPass (DBPool pool)
    hID   = HerokuID       . encodeUtf8 $ manifest ^. Manifest.id
    hPass = HerokuPassword . encodeUtf8 $ manifest ^. api ^. password

setupPool :: HasLogFunc cfg => RIO cfg SeldaPool
setupPool = do
  dbPathEnv <- liftIO $ lookupEnv "DB_PATH"
  dbPath'   <- return $ maybe "ipfs-api.sqlite" DBPath dbPathEnv
  SQLite.connPool dbPath'
