module Main (main) where

import RIO
import RIO.Process (mkDefaultProcessContext)

import Data.Aeson (decodeFileStrict)
import System.Envy

import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger

import Fission.Config.Types
import Fission.Storage.SQLite as SQLite

import           Fission.Environment
import           Fission.Internal.Orphanage ()
import           Fission.Storage.Types as DB
import qualified Fission.IPFS.Types    as IPFS
import qualified Fission.Monitor       as Monitor
import qualified Fission.Web           as Web
import qualified Fission.Web.Types     as Web
import qualified Fission.Web.Log       as Web.Log

import qualified Fission.Platform.Heroku.AddOn.Manifest as Manifest
import           Fission.Platform.Heroku.AddOn.Manifest hiding (id)
import qualified Fission.Platform.Heroku.Types          as Heroku

main :: IO ()
main = do
  Web.Port port <- getEnv
  Just manifest <- decodeFileStrict "./addon-manifest.json"

  _processCtx  <- mkDefaultProcessContext
  _host        <- decode .!~ Web.Host "localhost:1337"
  _ipfsPath    <- decode .!~ IPFS.BinPath "/usr/local/bin/ipfs"
  _ipfsTimeout <- decode .!~ IPFS.Timeout 150
  _dbPath      <- decode .!~ DB.Path "web-api.sqlite"
  _dbPool      <- RIO.runSimpleApp $ SQLite.connPool _dbPath

  condDebug   <- withFlag "PRETTY_REQS" id logStdoutDev
  isVerbose   <- getFlag "RIO_VERBOSE"
  logOptions' <- logOptionsHandle stdout isVerbose
  let logOpts = setLogUseTime True logOptions'

  withLogFunc logOpts $ \_logFunc -> do
    let
      _herokuID       = Heroku.ID       . encodeUtf8 $ manifest ^. Manifest.id
      _herokuPassword = Heroku.Password . encodeUtf8 $ manifest ^. api ^. password
      config          = Config {..}

    runRIO config do
      condMonitor
      logDebug $ "Servant port is " <> display port
      logDebug $ "Configured with: " <> displayShow config
      liftIO . runSettings (Web.Log.mkSettings _logFunc port) . condDebug =<< Web.app =<< ask

condMonitor :: HasLogFunc cfg => RIO cfg ()
condMonitor = do
  monitorFlag <- liftIO $ getFlag "MONITOR"
  when monitorFlag Monitor.wai
