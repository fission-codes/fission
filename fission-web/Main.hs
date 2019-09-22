module Main (main) where

import           RIO
import           RIO.Process (mkDefaultProcessContext)

import           Data.Aeson (decodeFileStrict)
import qualified Data.Yaml as Yaml

import qualified Network.HTTP.Client as HTTP
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           Network.Wai.Middleware.RequestLogger

import           Fission.Internal.Orphanage.RIO ()

import           Fission.Storage.PostgreSQL (connPool)
import qualified Fission.Monitor            as Monitor

import qualified Fission.Web       as Web
import qualified Fission.Web.CORS  as CORS
import qualified Fission.Web.Log   as Web.Log
import qualified Fission.Web.Types as Web

import qualified Fission.Platform.Heroku.AddOn.Manifest as Hku
import qualified Fission.Platform.Heroku.Types          as Hku

import           Fission.Config.Types
import           Fission.Environment
import           Fission.Environment.Types
import           Fission.IPFS.Environment.Types    as IPFS
import qualified Fission.Storage.Environment.Types as Storage
import qualified Fission.Web.Environment.Types     as Web

main :: IO ()
main = do
  Just  manifest <- decodeFileStrict "./addon-manifest.json"
  Yaml.decodeFileEither "./env.yaml" >>= \case
    Left err ->
      runSimpleApp . logError $ "failed with: " <> displayShow err

    Right env -> do
      let
        Storage.Environment {..} = env ^. storage
        Web.Environment     {..} = env ^. web

        _herokuID       = Hku.ID       . encodeUtf8 $ manifest ^. Hku.id
        _herokuPassword = Hku.Password . encodeUtf8 $ manifest ^. Hku.api ^. Hku.password

        _ipfsPath    = env ^. ipfs . binPath
        _ipfsURL     = env ^. ipfs . url
        _ipfsTimeout = env ^. ipfs . IPFS.timeout

      _dbPool      <- runSimpleApp $ connPool _stripeCount _connsPerStripe _connTTL _pgConnectInfo
      _processCtx  <- mkDefaultProcessContext
      _httpManager <- HTTP.newManager HTTP.defaultManagerSettings
      isVerbose    <- getFlag "RIO_VERBOSE" .!~ False -- TODO FISSION_VERBOSE or VERBOSE
      logOptions   <- logOptionsHandle stdout isVerbose

      withLogFunc (setLogUseTime True logOptions) $ \_logFunc -> runRIO Config {..} do
        let
          Web.Port port' = _port
          webLogger      = Web.Log.mkSettings _logFunc port'
          runner         = if env ^. web . Web.isTLS then runTLS tlsSettings' else runSettings
          condDebug      = if env ^. web . Web.pretty then id else logStdoutDev

        when (env ^. web . Web.monitor) Monitor.wai
        logDebug . displayShow =<< ask
        liftIO . runner webLogger
               . CORS.middleware
               . condDebug
               =<< Web.app
               =<< ask

tlsSettings' :: TLSSettings
tlsSettings' = tlsSettings "domain-crt.txt" "domain-key.txt"
