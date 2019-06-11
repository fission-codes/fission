module Main (main) where

import RIO
import RIO.Char (toLower)

import Data.Aeson
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import System.Environment

import Fission.Config         as Config
import Fission.Storage.SQLite as SQLite

import Fission.Internal.Orphanage ()

import qualified Fission.Log                            as Log
import qualified Fission.Monitor                        as Monitor
import qualified Fission.Web                            as Web
import qualified Fission.Web.Config                     as Web.Config
import           Fission.Platform.Heroku.AddOn.Manifest as Manifest

main :: IO ()
main = withStdoutLogger $ \stdOut -> do
  Just (manifest :: Manifest) <- decodeFileStrict "./addon-manifest.json"

  runRIO (mkLogFunc Log.simple) do
    Web.Config.Config {port} <- Web.Config.get
    pool <- setupPool

    let
      settings = mkSettings stdOut port
      config   = mkConfig manifest pool

    runRIO config do
      condMonitor
      logInfo $ "Servant running at port " <> display port
      liftIO . runSettings settings =<< Web.app config

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

mkConfig :: Manifest -> SeldaPool -> Config
mkConfig manifest pool = Config.base hID hPass (DBPool pool)
  where
    hID   = HerokuID       . encodeUtf8 $ manifest ^. Manifest.id
    hPass = HerokuPassword . encodeUtf8 $ manifest ^. api ^. password

setupPool :: HasLogFunc cfg => RIO cfg SeldaPool
setupPool = do
  dbPathEnv <- liftIO $ lookupEnv "DB_PATH"
  dbPath'   <- return $ maybe "ipfs-api.sqlite" DBPath dbPathEnv
  SQLite.connPool dbPath'
