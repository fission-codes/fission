module Main (main) where

import           RIO
import           RIO.Process (mkDefaultProcessContext)

import           Data.Aeson (decodeFileStrict)
import qualified Data.Yaml as Yaml
import           Servant.Client
import           System.Envy

import qualified Network.HTTP.Client as HTTP
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           Network.Wai.Middleware.RequestLogger

import           Fission.Config.Types

import           Fission.Environment
import           Fission.Internal.Orphanage.RIO ()
import           Fission.Internal.Orphanage.PGConnectInfo ()

import           Fission.Storage.Types      as DB
import           Fission.Storage.PostgreSQL as PG
import qualified Fission.IPFS.Types         as IPFS
import qualified Fission.Monitor            as Monitor

import qualified Fission.Web       as Web
import qualified Fission.Web.CORS  as CORS
import qualified Fission.Web.Log   as Web.Log
import qualified Fission.Web.Types as Web

import qualified Fission.Platform.Heroku.AddOn.Manifest as Manifest
import           Fission.Platform.Heroku.AddOn.Manifest hiding (id)
import qualified Fission.Platform.Heroku.Types          as Heroku






import Fission.Environment.Types
import Database.Selda.PostgreSQL

import qualified Fission.IPFS.Config.Types    as IPFSCfg
import qualified Fission.Storage.Config.Types as StorageCfg
import qualified Fission.Web.Config.Types     as WebCfg

main :: IO ()
main = do
  Just  manifest <- decodeFileStrict "./addon-manifest.json"
  Right env      <- Yaml.decodeFileEither "./env.yaml"

  let
    _port          = env ^. web     . WebCfg.port
    _host          = env ^. web     . WebCfg.host
    _ipfsPath      = env ^. ipfs    . IPFSCfg.binPath
    _ipfsTimeout   = env ^. ipfs    . IPFSCfg.timeout
    _ipfsURL       = env ^. ipfs    . IPFSCfg.url
    _pgConnectInfo = env ^. storage . StorageCfg.pgConnectInfo

  -- let _pgInfo = DB.PGInfo pgConnInfo
  _dbPool      <- RIO.runSimpleApp $ PG.connPool _pgInfo
  _processCtx  <- mkDefaultProcessContext
  _httpManager <- HTTP.newManager HTTP.defaultManagerSettings
  -- _host        <- decode .!~ Web.Host "https://runfission.com"
  -- _ipfsPath    <- decode .!~ IPFS.BinPath "/usr/local/bin/ipfs"
  -- _ipfsTimeout <- decode .!~ IPFS.Timeout 150
  -- ipfsURLRaw   <- withEnv "IPFS_URL" "http://localhost:5001" id
  -- _ipfsURL     <- IPFS.URL <$> parseBaseUrl ipfsURLRaw

  -- condDebug   <- withFlag "PRETTY_REQS" id logStdoutDev
  isVerbose   <- getFlag "RIO_VERBOSE" .!~ False -- TODO FISSION_VERBOSE or VERBOSE

  logOptions' <- logOptionsHandle stdout isVerbose
  let logOpts = setLogUseTime True logOptions'

  -- isTLS <- getFlag "TLS" .!~ True
  -- Web.Port port <- decode .!~ (Web.Port $ if isTLS then 443 else 80)

  withLogFunc logOpts $ \_logFunc -> do
    let
      _herokuID       = Heroku.ID       . encodeUtf8 $ manifest ^. Manifest.id
      _herokuPassword = Heroku.Password . encodeUtf8 $ manifest ^. api ^. password
      config          = Config {..}

    runRIO config do
      condMonitor
      logDebug $ "Servant port is " <> display _port
      logDebug $ "TLS is " <> if env ^. web . isTLS then "on" else "off"
      logDebug $ "Configured with: " <> displayShow config

      let runner = if isTLS
                      then runTLS (tlsSettings "domain-crt.txt" "domain-key.txt")
                      else runSettings

      liftIO . runner (Web.Log.mkSettings _logFunc _port)
             . CORS.middleware
             . condDebug
             =<< Web.app
             =<< ask

condMonitor :: HasLogFunc cfg => RIO cfg ()
condMonitor = do
  monitorFlag <- liftIO $ getFlag "MONITOR" .!~ False
  when monitorFlag Monitor.wai
