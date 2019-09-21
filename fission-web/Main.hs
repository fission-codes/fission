module Main (main) where

import           RIO
import           RIO.Process (mkDefaultProcessContext)

import           Data.Aeson (decodeFileStrict)
import qualified Data.Yaml as Yaml

import qualified Network.HTTP.Client as HTTP
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           Network.Wai.Middleware.RequestLogger

import           Fission.Config.Types

import           Fission.Environment
import           Fission.Internal.Orphanage.RIO ()
import           Fission.Internal.Orphanage.PGConnectInfo ()

import           Fission.Storage.PostgreSQL (connPool)
import qualified Fission.Monitor            as Monitor

import qualified Fission.Web       as Web
import qualified Fission.Web.CORS  as CORS
import qualified Fission.Web.Log   as Web.Log
-- import qualified Fission.Web.Types as Web

import qualified Fission.Platform.Heroku.AddOn.Manifest as Manifest
import           Fission.Platform.Heroku.AddOn.Manifest (api, password)
import qualified Fission.Platform.Heroku.Types          as Heroku

import Fission.Environment.Types

import qualified Fission.IPFS.Config.Types    as IPFS
import qualified Fission.Storage.Config.Types as Storage
import qualified Fission.Web.Config.Types     as Web

main :: IO ()
main = do
  Just  manifest <- decodeFileStrict "./addon-manifest.json"
  Right env      <- Yaml.decodeFileEither "./env.yaml"

  let
    Storage.Config {..} = env ^. storage
    Web.Config     {..} = env ^. web

    _herokuID       = Heroku.ID       . encodeUtf8 $ manifest ^. Manifest.id
    _herokuPassword = Heroku.Password . encodeUtf8 $ manifest ^. api ^. password

    _ipfsPath    = env ^. ipfs . IPFS.binPath
    _ipfsURL     = env ^. ipfs . IPFS.url
    _ipfsTimeout = env ^. ipfs . IPFS.timeout

  _dbPool      <- RIO.runSimpleApp $ connPool _stripeCount _connsPerStripe _connTTL _pgConnectInfo
  _processCtx  <- mkDefaultProcessContext
  _httpManager <- HTTP.newManager HTTP.defaultManagerSettings

  isVerbose  <- getFlag "RIO_VERBOSE" .!~ False -- TODO FISSION_VERBOSE or VERBOSE
  logOptions <- logOptionsHandle stdout isVerbose

  withLogFunc (setLogUseTime True logOptions) $ \_logFunc -> do
    let config = Config {..}

    runRIO config do
      when (env ^. web . Web.monitor) Monitor.wai
      logDebug $ "Configured with: " <> displayShow config

      let
        runner =
          if env ^. web . Web.isTLS
             then runTLS (tlsSettings "domain-crt.txt" "domain-key.txt")
             else runSettings

        condDebug = if env ^. web . Web.pretty then id else logStdoutDev

      liftIO . runner (Web.Log.mkSettings _logFunc (Web.port _port))
             . CORS.middleware
             . condDebug
             =<< Web.app
             =<< ask
