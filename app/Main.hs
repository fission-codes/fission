module Main (main) where

import RIO
import RIO.Process (mkDefaultProcessContext)

import Data.Aeson (decodeFileStrict)
import System.Envy

import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Network.Wai.Middleware.RequestLogger

import Fission.Config.Types
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

import Network.HTTP.Types.Status
import           Fission.Internal.Constraint
import Network.Wai

main :: IO ()
main = withStdoutLogger $ \stdOut -> do
  Web.Port port <- getEnv
  Just manifest <- decodeFileStrict "./addon-manifest.json"
  _processCtx   <- mkDefaultProcessContext
  condDebug     <- withFlag "DEBUG_REQS" id logStdoutDev
  _minLogLevel  <- decode .!~ Log.MinLevel LevelDebug
  _host         <- decode .!~ Web.Host "localhost:1337"
  _ipfsPath     <- decode .!~ IPFS.BinPath "/usr/local/bin/ipfs"
  _dbPath       <- decode .!~ DB.Path "ipfs-api.sqlite"
  _dbPool       <- RIO.runSimpleApp $ SQLite.connPool _dbPath

  let isVerbose = True -- get from the command line instead
  logOptions' <- logOptionsHandle stdout isVerbose
  let logOptions = setLogUseTime True logOptions'

  withLogFunc logOptions $ \_logFunc -> do
    let
      _herokuID       = Heroku.ID       . encodeUtf8 $ manifest ^. Manifest.id
      _herokuPassword = Heroku.Password . encodeUtf8 $ manifest ^. api ^. password
      config          = Config {..}

    runRIO config do
      condMonitor
      logDebug $ "Configured with: " <> displayShow config
      logInfo  $ "Servant running at: " <> display port
      liftIO . runSettings (mkSettings' _logFunc port) . condDebug =<< Web.app config

mkSettings :: ApacheLogger -> Port -> Settings
mkSettings stdOut port = portSettings $ logSettings defaultSettings
  where
    portSettings = setPort port
    logSettings  = setLogger stdOut

mkSettings' :: LogFunc -> Port -> Settings
mkSettings' logger port = defaultSettings
                        & setPort port
                        & setLogger (rioApacheLogger logger)

condMonitor :: HasLogFunc cfg => RIO cfg ()
condMonitor = do
  monitorFlag <- liftIO $ getFlag "MONITOR"
  when monitorFlag Monitor.wai

rioApacheLogger :: LogFunc -> ApacheLogger
rioApacheLogger logger = \req status@(Status {..}) _mayInt ->
  runRIO logger $ output req statusCode -- (Request -> Status -> Maybe Integer -> IO ())

output :: (MonadRIO cfg m, HasLogFunc cfg) => Request -> Int -> m ()
output req statusCode | statusCode >= 500 = logError $ displayShow req
                      | statusCode >= 400 = logInfo $ displayShow req
                      | otherwise        = logDebug $ displayShow req

withStdoutLogger :: (ApacheLogger -> IO a) -> IO a
withStdoutLogger app = bracket setup teardown $ \(aplogger, _) ->
    app aplogger
  where
    setup = do
      tgetter <- newTimeCache simpleTimeFormat
      apf     <- initLogger FromFallback (LogStdout 4096) tgetter
      let aplogger = apacheLogger apf
          remover  = logRemover apf
      return (aplogger, remover)
    teardown (_, remover) = void remover
