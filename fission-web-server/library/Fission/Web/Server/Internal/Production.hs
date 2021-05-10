module Fission.Web.Server.Internal.Production
  ( start
  , runInProd
  , runInProdSimple
  , mkSettings
  , tlsSettings
  , clientTimeout
  , serverTimeout
  ) where

import           Control.Exception

import qualified Data.Aeson                                      as JSON
import qualified Data.Yaml                                       as YAML

import           Servant
import qualified Servant.Ekg                                     as EKG

import qualified System.Metrics                                  as EKG

import qualified Network.HTTP.Client                             as HTTP
import qualified Network.HTTP.Client.TLS                         as HTTP
import qualified Network.HTTP.Types.Header                       as HTTP

import qualified Network.HostName                                as Network
import           Network.IPFS.Timeout.Types                      as IPFS

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           Network.Wai.Middleware.RequestLogger

import qualified RIO.ByteString                                  as BS
import qualified RIO.Text                                        as Text

import           Fission.Prelude

import           Fission.Internal.App
import           Fission.Time
import           Fission.User.DID.Types

import qualified Fission.Web.Server.Types                        as Fission
import qualified Fission.Web.Server.Types                        as Fission.Server

import qualified Fission.Web.Server.DID.Publicize.Class          as ServerDID
import           Fission.Web.Server.Domain                       as Domain
import           Fission.Web.Server.MonadDB
import           Fission.Web.Server.Storage.PostgreSQL
import           Fission.Web.Server.User                         as User

import qualified Fission.Web.Server                              as Fission.Server
import qualified Fission.Web.Server.Error                        as Web.Error
import           Fission.Web.Server.Host.Types                   as Server
import qualified Fission.Web.Server.Log                          as Web.Log

import           Fission.Web.Server.Auth                         as Auth
import           Fission.Web.Server.Handler
import qualified Fission.Web.Server.Middleware.CORS              as CORS

import qualified Fission.Web.Server.Heroku.AddOn.Manifest.Types  as Hku
import qualified Fission.Web.Server.Heroku.ID.Types              as Hku
import qualified Fission.Web.Server.Heroku.Password.Types        as Hku
import qualified Fission.Web.Server.Sentry                       as Sentry

import qualified Fission.Web.Server.Environment.AWS.Types        as AWS
import qualified Fission.Web.Server.Environment.Auth.Types       as Auth
import           Fission.Web.Server.Environment.IPFS.Types       as IPFS
import qualified Fission.Web.Server.Environment.SendInBlue.Types as SendInBlue
import qualified Fission.Web.Server.Environment.Server.Types     as Server
import qualified Fission.Web.Server.Environment.Storage.Types    as Storage
import           Fission.Web.Server.Environment.Types
import qualified Fission.Web.Server.Environment.WNFS.Types       as WNFS
import qualified Fission.Web.Server.Environment.WebApp.Types     as WebApp

import           Fission.Web.Server.Internal.Orphanage.WebSocket ()

runInProdSimple :: Fission.Server () -> IO ()
runInProdSimple action =
  runInProd (Just True) \_ _ -> do
    logDebug @Text "🌱🐇 Running in simple prod environment ✨🍭"
    action

runInProd
  :: Maybe Bool
  -> (  (Application -> Application)
     -> (Application -> IO ())
     -> Fission.Server a
     )
  -> IO a
runInProd overrideVerbose action = do
  putStrLnIO "🏗️  Setting up production environment..."

  putStrLnIO "   📥 Loading Heroku addon manifest..."
  Just manifest <- JSON.decodeFileStrict "./addon-manifest.json"

  putStrLnIO "   📥 Loading the Fission Server's env.yaml"
  env <- YAML.decodeFileThrow  "./env.yaml"

  let
    AWS.Environment        {..} = env |> aws
    Auth.Environment       {..} = env |> auth
    WNFS.Environment       {..} = env |> wnfs
    Server.Environment     {..} = env |> server
    Storage.Environment    {..} = env |> storage
    WebApp.Environment     {..} = env |> webApp
    SendInBlue.Environment {..} = env |> sendInBlue

    herokuID       = Hku.ID       . encodeUtf8 $ Hku.id manifest
    herokuPassword = Hku.Password . encodeUtf8 . Hku.password $ Hku.api manifest

    ipfsPath                                = env |> ipfs |> binPath
    ipfsURLs                                = env |> ipfs |> urls
    ipfsRemotePeers                         = env |> ipfs |> remotePeers
    ipfsTimeout@(IPFS.Timeout ipfsTOutSecs) = env |> ipfs |> IPFS.timeout

    awsAccessKey   = accessKey
    awsSecretKey   = secretKey
    awsMockRoute53 = mockRoute53

    userZoneID     = baseUserDataZoneID
    userRootDomain = baseUserDataRootDomain

  putStrLnIO "   🦜 Setting verbosity"
  isVerbose' <- isDebugEnabled
  let isVerbose = fromMaybe isVerbose' overrideVerbose
  logOptions <- logOptionsHandle stdout isVerbose

  putStrLnIO "   📠 Setting up Unix process context"
  processCtx <- mkDefaultProcessContext

  putStrLnIO "   🕷️  Setting up HTTP manager"
  putStrLnIO "      🎛️  Configuring..."

  let
    httpSettings = HTTP.defaultManagerSettings
      { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro clientTimeout
      , HTTP.managerConnCount       = 200
      }

    tlsHttpSettings = HTTP.tlsManagerSettings
      { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro clientTimeout
      , HTTP.managerConnCount       = 200
      }

    Seconds (Micro ipfsTOut) = convert $ Seconds (Unity ipfsTOutSecs)
    ipfsHttpSettings = HTTP.defaultManagerSettings
      { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro $ fromIntegral ipfsTOut
      , HTTP.managerConnCount       = 1000
      }

  putStrLnIO "      📞🌐 Creating base HTTP client manager"
  httpManager <- HTTP.newManager httpSettings

  putStrLnIO "      🛸🌐 Creating IPFS HTTP client manager"
  ipfsHttpManager <- HTTP.newManager ipfsHttpSettings

  putStrLnIO "      🙈🌐 Creating TLS client manager"
  tlsManager <- HTTP.newManager tlsHttpSettings

  putStrLnIO "   💂 Configuring optional Sentry middleware"
  condSentryLogger <- maybe (pure mempty) (Sentry.mkLogger host environment) sentryDSN

  putStrLnIO "   📛 Getting machine name"
  machineName <- Network.getHostName

  putStrLnIO "   📋 Setting up application logger"
  withLogFunc (setLogUseTime True logOptions) \baseLogger -> do
    ekg <- liftIO . EKG.monitorEndpoints api =<< EKG.newStore

    let
      condEKG      = if True then ekg else identity
      condDebug    = if pretty then identity else logStdoutDev
      middleware   = condEKG . condDebug

      runSettings' = if isTLS then runTLS tlsSettings' else runSettings
      runner       = runSettings' $ mkSettings logFunc port
      logFunc      = baseLogger <> condSentryLogger

    putStrLnIO "   🏊 Establishing database pool"
    withDBPool baseLogger pgConnectInfo (PoolSize 12) \dbPool -> do
      putStrLnIO "   🔌 Setting up websocket relay store"
      linkRelayStoreVar <- atomically $ newTVar mempty

      putStrLnIO "✅ Setup done, running action(s)..."
      Fission.Server.runServer Fission.Server.Config {..} (action middleware runner)

start :: (Application -> Application) -> (Application -> IO ()) -> Fission.Server ()
start middleware runner = do
  now <- currentTime
  cfg@Fission.Server.Config {..} <- ask

  let DID _ serverPK = fissionDID

  logDebug $ displayShow cfg

  runDB do
    logInfo @Text "🗂️  Ensuring live DB matches latest schema"
    updateDBToLatest

    logInfo @Text "🙋 Ensuring default user is in DB"
    userId <- User.getByPublicKey serverPK >>= \case
      Just (Entity userId _) -> return userId
      Nothing -> Web.Error.ensureM $ User.createDB "fission" serverPK "hello@fission.codes" now

    logInfo @Text "💽 Ensuring default data domain domains is in DB"
    Domain.getByDomainName userRootDomain >>= \case
      Right _ -> return ()
      Left  _ -> Domain.create userRootDomain userId userZoneID now

    logInfo @Text "📱 Ensuring default app domain domains is in DB"
    Domain.getByDomainName baseAppDomain >>= \case
      Right _ -> return ()
      Left  _ -> Domain.create baseAppDomain userId baseAppZoneID now

  auth <- Auth.mkAuth
  logDebug @Text $ layoutWithContext api auth

  logInfo $ "📤 Staring server at " <> Text.pack (show now)
  Web.Error.ensureM ServerDID.publicize

  host
    |> Fission.Server.app (toHandler (Fission.Server.runServer cfg)) auth
    |> middleware
    |> CORS.middleware
    |> runner
    |> liftIO

api :: Proxy Fission.Server.API
api = Proxy

mkSettings :: LogFunc -> Server.Port -> Settings
mkSettings logger (Server.Port port) =
  defaultSettings
    |> setPort port
    |> setLogger (Web.Log.fromLogFunc logger)
    |> setOnExceptionResponse onExceptionResponse
    |> setTimeout serverTimeout

tlsSettings' :: TLSSettings
tlsSettings' = tlsSettings "domain-crt.txt" "domain-key.txt"

clientTimeout :: Int
clientTimeout = 540000000 -- 9 minutes = 1 min less than AWS

serverTimeout :: Int
serverTimeout = 1800

putStrLnIO :: MonadIO m => Text -> m ()
putStrLnIO txt = BS.putStr (encodeUtf8 txt <> "\n")

addResponseHeader :: HTTP.HeaderName -> ByteString -> Response -> Response
addResponseHeader headerName value = mapResponseHeaders \headers -> (headerName, value) : headers

onExceptionResponse :: SomeException -> Response
onExceptionResponse exception =
  addResponseHeader "Access-Control-Allow-Origin" "*" (defaultOnExceptionResponse exception)
