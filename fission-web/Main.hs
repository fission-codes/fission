module Main (main) where

import           RIO
import           RIO.ByteString (putStr)
import           RIO.Process (mkDefaultProcessContext)

import qualified Data.Aeson as JSON
import           Data.Aeson.Encode.Pretty
import qualified Data.Yaml  as YAML

import qualified Network.HTTP.Client as HTTP
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           Network.Wai.Middleware.RequestLogger

import           SuperRecord as SR

import           Fission.Internal.Orphanage.Rec ()
import           Fission.Internal.Orphanage.RIO ()

import qualified Fission.Monitor            as Monitor
import           Fission.Storage.PostgreSQL (connPool)

import qualified Fission.Web      as Web
import qualified Fission.Web.CORS as CORS
import qualified Fission.Web.Log  as Web.Log

import qualified Fission.Platform.Heroku.AddOn.Manifest as Hku
import qualified Fission.Platform.Heroku.Types          as Hku

import           Fission.Environment
import           Fission.Environment.Types
import           Fission.IPFS.Environment.Types    as IPFS
import qualified Fission.Storage.Environment.Types as Storage
import qualified Fission.Web.Environment.Types     as Web
import qualified Fission.Web.Environment.Types     as WebEnv

main :: IO ()
main = do
  Just  manifest <- JSON.decodeFileStrict "./addon-manifest.json"
  Right env      <- YAML.decodeFileEither "./env.yaml"

  let Storage.Environment {..} = env ^. storage

  processCtx  <- mkDefaultProcessContext
  httpManager <- HTTP.newManager HTTP.defaultManagerSettings
  isVerbose   <- getFlag "RIO_VERBOSE" .!~ False
  logOptions  <- logOptionsHandle stdout isVerbose
  withLogFunc (setLogUseTime True logOptions) \logFunc -> do

    let
      logger = #logFunc := logFunc

      visibleRec = #ipfsPath       := (env ^. ipfs . binPath)
              SR.& #ipfsURL        := (env ^. ipfs . url)
              SR.& #ipfsTimeout    := (env ^. ipfs . IPFS.timeout)
              SR.& #host           := (env ^. web  . Web.host)
              SR.& rnil

    dbPool <- runRIO (logger SR.& rnil) $ connPool _stripeCount _connsPerStripe _connTTL _pgConnectInfo

    let
      cfg =    #processCtx  := processCtx
          SR.& #httpManager := httpManager
          SR.& #dbPool      := dbPool
          SR.& #herokuID       := (Hku.ID       . encodeUtf8 $ manifest ^. Hku.id)
          SR.& #herokuPassword := (Hku.Password . encodeUtf8 $ manifest ^. Hku.api ^. Hku.password)
          SR.& logger
          SR.& visibleRec

    runRIO cfg do
      logDebug $ display $ encodePretty visibleRec

      let
        webLogger = Web.Log.mkSettings logFunc (env ^. web . WebEnv.port)
        runner    = if env ^. web . Web.isTLS then runTLS tlsSettings' else runSettings
        condDebug = if env ^. web . Web.pretty then id else logStdoutDev

      when (env ^. web . Web.monitor) Monitor.wai
      liftIO . runner webLogger
            . CORS.middleware
            . condDebug
            =<< Web.app
            =<< ask

tlsSettings' :: TLSSettings
tlsSettings' = tlsSettings "domain-crt.txt" "domain-key.txt"
