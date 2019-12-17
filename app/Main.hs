module Main (main) where


import qualified Data.Aeson as JSON
import qualified Data.Yaml  as YAML

import qualified Network.HTTP.Client as HTTP

import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           Network.Wai.Middleware.RequestLogger

import qualified RIO

import           Fission.App
import           Fission.Config.Types
import           Fission.Prelude
import           Fission.Internal.Orphanage.RIO ()
import           Fission.Storage.PostgreSQL

import qualified Fission.Web       as Web
import qualified Fission.Web.CORS  as CORS
import qualified Fission.Web.Log   as Web.Log
import qualified Fission.Web.Types as Web

import qualified Fission.Platform.Heroku.AddOn.Manifest.Types as Hku
import qualified Fission.Platform.Heroku.ID.Types             as Hku
import qualified Fission.Platform.Heroku.Password.Types       as Hku

import           Fission.Environment.IPFS.Types    as IPFS
import           Fission.Environment.Types
import qualified Fission.Environment.AWS.Types     as AWS
import qualified Fission.Environment.Storage.Types as Storage
import qualified Fission.Environment.Web.Types     as Web

import qualified Fission.Web.Log.Sentry as Sentry
import qualified Fission.Monitor        as Monitor

main :: IO ()
main = do
  Just  manifest <- JSON.decodeFileStrict "./addon-manifest.json"
  env            <- YAML.decodeFileThrow  "./env.yaml"

  let
    Storage.Environment {..} = env |> storage
    Web.Environment     {..} = env |> web
    AWS.Environment     {..} = env |> aws

    herokuID       = Hku.ID       <| encodeUtf8 <| Hku.id <| manifest
    herokuPassword = Hku.Password <| encodeUtf8 <| Hku.password <| Hku.api <| manifest

    ipfsPath       = env |> ipfs |> binPath
    ipfsURL        = env |> ipfs |> url
    ipfsRemotePeer = env |> ipfs |> remotePeer
    ipfsTimeout    = env |> ipfs |> IPFS.timeout
    ipfsGateway    = env |> ipfs |> gateway

    awsAccessKey          = accessKey
    awsSecretKey          = secretKey
    awsZoneID             = zoneID
    awsDomainName         = domainName
    awsRoute53MockEnabled = route53MockEnabled

  isVerbose  <- isDebugEnabled
  logOptions <- logOptionsHandle stdout isVerbose

  processCtx  <- mkDefaultProcessContext
  httpManager <- HTTP.newManager HTTP.defaultManagerSettings
                   { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro clientTimeout }

  condSentryLogger <- maybe (pure mempty) (Sentry.mkLogger RIO.LevelWarn) sentryDSN

  withLogFunc (setLogUseTime True logOptions) \baseLogger -> do
    let
      logFunc        = baseLogger <> condSentryLogger
      Web.Port port' = port
      settings       = mkSettings logFunc port'
      runner         = if env |> web |> Web.isTLS then runTLS tlsSettings' else runSettings
      condDebug      = if env |> web |> Web.pretty then identity else logStdoutDev

    withDBPool baseLogger pgConnectInfo 4 \dbPool -> runRIO Config {..} do
      logDebug . displayShow =<< ask
      when (env |> web |> Web.monitor) Monitor.wai

      logInfo ("Ensuring live DB matches latest schema" :: Text)
      runDB updateDBToLatest

      app <- Web.app

      app
        |> condDebug
        |> CORS.middleware
        |> runner settings
        |> liftIO

mkSettings :: LogFunc -> Port -> Settings
mkSettings logger port =
  defaultSettings
    |> setPort port
    |> setLogger (Web.Log.fromLogFunc logger)
    |> setTimeout serverTimeout

tlsSettings' :: TLSSettings
tlsSettings' = tlsSettings "domain-crt.txt" "domain-key.txt"

clientTimeout :: Int
clientTimeout = 1800000000

serverTimeout :: Int
serverTimeout = 1800
