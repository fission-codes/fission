module Main (main) where

import RIO

import Data.Aeson (decodeFileStrict)
import System.Envy

import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Network.Wai.Middleware.RequestLogger

import Fission
import Fission.Types
import Fission.Storage.SQLite as SQLite

import           Fission.Environment
import           Fission.Internal.Orphanage ()
import           Fission.Storage.Types as DB
import qualified Fission.IPFS.Types    as IPFS
import qualified Fission.Log           as Log
import qualified Fission.Monitor       as Monitor
import qualified Fission.Web           as Web
import qualified Fission.Web.Types     as Web

import qualified Fission.Platform.Heroku.AddOn.Manifest as Manifest
import           Fission.Platform.Heroku.AddOn.Manifest hiding (id)
import qualified Fission.Platform.Heroku.Types          as Heroku

main :: IO ()
main = withStdoutLogger $ \stdOut -> do
  Web.Port port <- getEnv
  Just manifest <- decodeFileStrict "./addon-manifest.json"
  condDebug     <- withFlag "DEBUG_REQS" id logStdoutDev
  _minLogLevel  <- decode .!~ Log.MinLevel LevelDebug
  _host         <- decode .!~ Web.Host "localhost:1337"
  _ipfsPath     <- decode .!~ IPFS.Path "/usr/local/bin/ipfs"
  _dbPath       <- decode .!~ DB.Path "ipfs-api.sqlite"
  _dbPool       <- simply $ SQLite.connPool _dbPath

  let
    _herokuID       = Heroku.ID       . encodeUtf8 $ manifest ^. Manifest.id
    _herokuPassword = Heroku.Password . encodeUtf8 $ manifest ^. api ^. password
    _logFunc        = mkLogFunc Log.simple
    config          = Config { .. }

  runRIO config do
    condMonitor
    logDebug $ "Configured with: " <> displayShow config
    logInfo  $ "Servant running at: " <> display port
    liftIO . runSettings (mkSettings stdOut port) . condDebug =<< Web.app config

mkSettings :: ApacheLogger -> Port -> Settings
mkSettings stdOut port = portSettings $ logSettings defaultSettings
  where
    portSettings = setPort port
    logSettings  = setLogger stdOut

condMonitor :: HasLogFunc cfg => RIO cfg ()
condMonitor = do
  monitorFlag <- liftIO $ getFlag "MONITOR"
  when monitorFlag Monitor.wai
