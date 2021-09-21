module Fission.CLI.Handler.User.Login
  ( LoginConstraints
  , login

  -- * Consumer
  , ConsumerConstraints
  , consume

  -- * Producer
  , ProducerConstraints
  , produce
  ) where

-- 📦 External

import           Crypto.Cipher.AES                           (AES256)
import qualified Crypto.PubKey.Ed25519                       as Ed25519
import qualified Crypto.PubKey.RSA.Types                     as RSA
import           Crypto.Random

import qualified Data.Yaml                                   as YAML

import qualified Network.DNS                                 as DNS
import qualified Network.HTTP.Client                         as HTTP
import           Network.IPFS.CID.Types
import           Servant.Client.Core

-- ⚛️  Fission

import           Fission.Prelude

import qualified Fission.Internal.UTF8                       as UTF8

import           Fission.Error.Types
import qualified Fission.JSON                                as JSON

import           Fission.Authorization.ServerDID.Class
import qualified Fission.Key.Asymmetric.Algorithm.Error      as Asymmetric.Algorithm
import           Fission.Key.Asymmetric.Public.Types
import qualified Fission.Key.Error                           as Key
import qualified Fission.Key.Symmetric                       as Symmetric

import           Fission.User.DID.NameService.Class          as DID
import           Fission.User.DID.Types                      as DID
import           Fission.User.Username                       as Username

import           Fission.Web.Client
import qualified Fission.Web.Serialization                   as Web.Serialization

-- 🛂 JWT/UCAN

import           Fission.Web.Auth.Token.Bearer               as Bearer
import           Fission.Web.Auth.Token.JWT                  as JWT
import qualified Fission.Web.Auth.Token.JWT.Claims.Error     as JWT.Claims
import           Fission.Web.Auth.Token.JWT.Fact.Types
import qualified Fission.Web.Auth.Token.JWT.Proof            as UCAN
import qualified Fission.Web.Auth.Token.JWT.Proof.Error      as JWT.Proof
import qualified Fission.Web.Auth.Token.JWT.Resolver.Class   as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Error   as UCAN.Resolver
import qualified Fission.Web.Auth.Token.JWT.Validation       as UCAN
import           Fission.Web.Auth.Token.Types                as Web

-- 📟 CLI

import           Fission.CLI.Environment                     as Env
import           Fission.CLI.Key.Store                       as Key.Store
import           Fission.CLI.Linking.Status
import           Fission.CLI.PIN                             as PIN
import           Fission.CLI.Prompt
import           Fission.CLI.Remote
import           Fission.CLI.User
import qualified Fission.CLI.User.Link.Payload.Types         as User.Link

import           Fission.CLI.PubSub                          as PubSub
import           Fission.CLI.PubSub.Secure                   as Secure
import           Fission.CLI.PubSub.Secure.Payload           as SecurePayload
import qualified Fission.CLI.PubSub.Secure.Session.Types     as PubSub

-- 🕸️  WebNative

import qualified Fission.CLI.WebNative.FileSystem.Auth.Store as WebNative.FileSystem.Auth.Store
import qualified Fission.CLI.WebNative.Mutation.Auth.Store   as WebNative.Mutation.Store

type LoginConstraints m =
  ( MonadIO     m
  , MonadRemote m

  , m `Raises` Key.Error
  , m `Raises` ClientError

  , CheckErrors m
  , AlreadyExists DID `IsMember` Errors m

  , ConsumerConstraints m
  , ProducerConstraints m
  )

login :: LoginConstraints m => Maybe Username -> m Username
login optUsername = do
  signingSK <- Key.Store.fetch $ Proxy @SigningKey
  rootURL   <- getRemoteBaseUrl

  let
    -- NOTE hardcoded and not using safeLink since that would cause a dependency
    -- on fission-web-server. servant-websockets contains servant-server,
    -- and thus depends on zlib, which is annoying for our (potential) static builds.
    -- This may change in the future.
    baseURL = rootURL {baseUrlPath = "/user/link"}

  attempt ensureNotLoggedIn >>= \case
    Right () -> consume signingSK baseURL optUsername
    Left  _  -> produce signingSK baseURL

type ConsumerConstraints m =
  ( MonadIO          m
  , MonadTime        m
  , MonadLogger      m
  , MonadRandom      m
  , MonadEnvironment m
  , MonadNameService m
  , MonadWebClient   m
  , JWT.Resolver     m

  , WebNative.FileSystem.Auth.Store.MonadStore m
  , WebNative.Mutation.Store.MonadStore        m

  , MonadSecured m (Symmetric.Key AES256) PIN.Payload
  , MonadSecured m (Symmetric.Key AES256) User.Link.Payload
  , MonadSecured m (RSA.PublicKey, RSA.PrivateKey) PubSub.Session

  , MonadPubSubSecure m (RSA.PublicKey, RSA.PrivateKey)

  , MonadCleanup m
  , m `Raises` AlreadyExists Env
  , m `Raises` ClientError
  , m `Raises` DNS.DNSError
  , m `Raises` JSON.Error
  , m `Raises` JWT.Error
  , m `Raises` JWT.Proof.Error
  , m `Raises` NotFound DID
  , m `Raises` SecurePayload.Error
  , m `Raises` UCAN.Resolver.Error
  , m `Raises` Username.Invalid

  , Show (OpenUnion (Errors m))
  , Display (SecurePayload (Symmetric.Key AES256) User.Link.Payload)
  )

consume :: ConsumerConstraints m => Ed25519.SecretKey -> BaseUrl -> Maybe Username -> m Username
consume signingSK baseURL optUsername = do
  logDebug @Text "🛂📥 Consuming log-in..."
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings

  username <- do
    case optUsername of
      Just uName -> return uName
      Nothing    -> ensure . mkUsername =<< reaskNotEmpty' "📛 Please enter your username:"

  targetDID <- ensureM $ DID.getByUsername username
  signingPK <- Key.Store.toPublic (Proxy @SigningKey) signingSK

  let
    myDID = DID.Key (Ed25519PublicKey signingPK)
    topic = PubSub.Topic $ textDisplay targetDID

  PubSub.connect baseURL topic \conn -> reattempt 10 do
    logDebug @Text "🤝 Device linking handshake: Step 1"
    aesConn <- secure conn () \(rsaConn :: Secure.Connection m (RSA.PublicKey, RSA.PrivateKey)) -> reattempt 10 do
      let
        Secure.Connection {key = (pk, _sk)} = rsaConn
        sessionDID = DID.Key (RSAPublicKey pk)

      logDebug @Text "🤝 Device linking handshake: Step 2"
      broadcastApiData conn sessionDID

      reattempt 10 do
        logDebug @Text "🤝 Device linking handshake: Step 3"
        PubSub.Session
          { bearerToken = Bearer.Token {jwt, rawContent}
          , sessionKey
          } <- secureListenJSON rsaConn


        logDebug @Text "🤝 Device linking handshake: Step 4"
        ensureM $ UCAN.check manager sessionDID rawContent jwt

        -- TODO waiting on FE to not send an append UCAN -- case (jwt |> claims |> potency) == AuthNOnly of
        ensure $ UCAN.containsFact jwt \facts ->
          if SessionKey sessionKey `elem` facts
            then Right ()
            else Left JWT.Proof.MissingExpectedFact

        return Secure.Connection {conn, key = sessionKey}

    logDebug @Text "🤝 Device linking handshake: Step 5"
    pin <- PIN.create

    let
      pinStep = PIN.Payload myDID pin

    logUser $ "🔢 Confirmation code: " <> textDisplay pin
    secureBroadcastJSON aesConn pinStep

    reattempt 100 do
      logDebug @Text "🤝 Device linking handshake: Step 6"
      User.Link.Payload
        { bearer = bearer@Bearer.Token {jwt, rawContent}
        , readKey
        } <- secureListenJSON aesConn

      ensureM $ UCAN.check manager myDID rawContent jwt
      JWT {claims = JWT.Claims {sender}} <- ensureM $ getRoot jwt

      unless (sender == targetDID) do
        raise $ JWT.ClaimsError JWT.Claims.IncorrectSender

      _   <- WebNative.FileSystem.Auth.Store.set targetDID "/" readKey
      cid <- WebNative.Mutation.Store.insert bearer

      Env.init username baseURL (Just cid)

  return username

type ProducerConstraints m =
  ( MonadIO          m
  , MonadTime        m
  , MonadLogger      m
  , MonadRandom      m
  , MonadWebClient   m
  , MonadEnvironment m
  , MonadWebAuth     m Web.Token
  , MonadWebAuth     m Ed25519.SecretKey
  , ServerDID        m
  , JWT.Resolver     m

  , WebNative.FileSystem.Auth.Store.MonadStore m
  , WebNative.Mutation.Store.MonadStore        m

  , MonadSecured m (Symmetric.Key AES256)          PIN.Payload
  , MonadSecured m (Symmetric.Key AES256)          User.Link.Payload
  , MonadSecured m (RSA.PublicKey, RSA.PrivateKey) PubSub.Session

  , MonadPubSubSecure m (RSA.PublicKey, RSA.PrivateKey)
  , MonadPubSubSecure m (Symmetric.Key AES256)

  , MonadCleanup m
  , m `Raises` Asymmetric.Algorithm.Invalid
  , m `Raises` YAML.ParseException
  , m `Raises` JSON.Error
  , m `Raises` NotFound CID
  , m `Raises` NotFound FilePath
  , m `Raises` NotFound (Symmetric.Key AES256)
  , m `Raises` Web.Serialization.Error
  , m `Raises` SecurePayload.Error
  , m `Raises` UCAN.Resolver.Error
  , m `Raises` Mismatch PIN
  , m `Raises` ClientError
  , m `Raises` Status Denied

  , Display (SecurePayload (Symmetric.Key AES256) PIN.Payload)
  )

produce :: ProducerConstraints m => Ed25519.SecretKey -> BaseUrl -> m Username
produce signingSK baseURL = do
  logInfo @Text "🛂📤 Producing log-in..."

  signingPK      <- Key.Store.toPublic (Proxy @SigningKey) signingSK
  rootProof      <- WebNative.Mutation.Store.getRootUserProof
  rootDID        <- getRootDID (Ed25519PublicKey signingPK) rootProof
  Env {username} <- Env.get

  PubSub.connect baseURL (PubSub.Topic $ textDisplay rootDID) \conn -> reattempt 10 do
    logDebug @Text "🤝 Device linking handshake: Step 1 (noop)"

    logUser $ "🌐 Listening for logins requests for " <> textDisplay username

    secure conn () \(rsaConn@Secure.Connection {key = (_, sk)} :: Secure.Connection m (RSA.PublicKey, RSA.PrivateKey)) -> reattempt 10 do
      logDebug @Text "🤝 Device linking handshake: Step 2"
      requestorTempDID <- listenRaw conn

      case requestorTempDID of
        DID.ION _ ->
          error "ION is not a exchange key" -- FIXME

        DID.Key (Ed25519PublicKey _) ->
          raise Asymmetric.Algorithm.Invalid

        DID.Key (RSAPublicKey tmpRSA) ->
          reattempt 10 do
            logDebug @Text "🤝 Device linking handshake: Step 3"

            secure conn () \aesConn@Secure.Connection {key = sessionKey} -> do
              now <- getCurrentTime

              let
                connectedRSAConn = rsaConn {key = (tmpRSA, sk)}
                handshakeJWT = proveWNFS now requestorTempDID signingSK [SessionKey sessionKey] rootProof

              connectedRSAConn `secureBroadcastJSON` PubSub.Session { sessionKey
                                                                    , bearerToken = Bearer.fromJWT handshakeJWT
                                                                    }

              logDebug @Text "🤝 Device linking handshake: Step 4 (noop)"
              logDebug @Text "🤝 Device linking handshake: Step 5"
              PIN.Payload requestorDID pin <- secureListenJSON aesConn

              pinOK <- reaskYN $ "🔢  Does this code match your second device? " <> textDisplay pin
              unless pinOK do
                raise $ Mismatch @PIN

              reattempt 100 do
                logDebug @Text "🤝 Device linking handshake: Step 6"

                readKey <- do
                  attempt (WebNative.FileSystem.Auth.Store.getMostPrivileged rootDID "/") >>= \case
                    Left _ ->
                      case rootProof of
                        RootCredential -> WebNative.FileSystem.Auth.Store.create rootDID "/"
                        _              -> raise $ NotFound @(Symmetric.Key AES256)

                    Right (_, key) ->
                      return key

                let
                  jwt    = delegateSuperUser requestorDID signingSK rootProof now
                  bearer = Bearer.fromJWT jwt

                accessOK <- reaskYN $ "🧞 Grant access? " <> JWT.prettyPrintGrants jwt
                unless accessOK $ raise (Status Denied)

                aesConn `secureBroadcastJSON` User.Link.Payload {bearer, readKey}

    UTF8.putTextLn "🤝 Login to other device successful 🎉"

  return username
