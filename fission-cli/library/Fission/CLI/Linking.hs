module Fission.CLI.Linking where

import qualified Fission.Internal.Orphanage.ClientM               ()
import qualified Fission.Key                                      as Key

import qualified Crypto.PubKey.Ed25519                            as Ed25519
import           Fission.CLI.Linking.Status.Types
import qualified Fission.Internal.Base64.URL                      as B64.URL
import qualified Fission.Web.Auth.Token.JWT.Header                as Header

import           Fission.Authorization                            as Authorization
import           Fission.Key.Asymmetric.Algorithm.Types           as Key

import           Fission.Web.Auth.Token.UCAN.Resource.Types

import qualified Fission.Web.Auth.Token.Bearer.Types              as Bearer
import           Fission.Web.Auth.Token.JWT                       as JWT
import qualified Fission.Web.Auth.Token.JWT.Header.Typ.Types      as JWT.Typ
import qualified Fission.Web.Auth.Token.JWT.Signature.Types       as JWT.Signature
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types


import qualified System.Console.ANSI                              as ANSI

import           Crypto.Cipher.AES                                (AES256)
import           Crypto.Cipher.Types
import           Crypto.Error                                     (CryptoError (..),
                                                                   CryptoFailable (..))
import           Crypto.Hash.Algorithms
import qualified Crypto.PubKey.RSA.OAEP                           as RSA.OAEP
import qualified Crypto.PubKey.RSA.Types                          as RSA
import           Crypto.Random.Types                              as CRT

import           Data.ByteArray                                   as ByteArray

import qualified RIO.ByteString                                   as BS
import qualified RIO.ByteString.Lazy                              as Lazy
import qualified RIO.Text                                         as Text

import           Network.IPFS.Local.Class                         as IPFS
import qualified Network.IPFS.Process.Error                       as IPFS.Process

import           Fission.Prelude

import qualified Fission.Internal.UTF8                            as UTF8

import           Fission.Challenge.Types
import           Fission.Security.EncryptedWith.Types

import           Fission.IPFS.PubSub.Topic
import           Fission.User.DID.Types                           as DID

import           Fission.Key.Asymmetric.Public.Types
import qualified Fission.Key.Symmetric                            as Symmetric

import qualified Fission.IPFS.PubSub.Subscription                 as IPFS.PubSub.Subscription
import qualified Fission.IPFS.PubSub.Subscription                 as Sub
import qualified Fission.IPFS.PubSub.Topic                        as IPFS.PubSub

import           Fission.Authorization.Potency.Types
import           Fission.Web.Auth.Token.JWT                       as JWT
import qualified Fission.Web.Auth.Token.JWT                       as UCAN
import qualified Fission.Web.Auth.Token.JWT.Error                 as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Class        as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Error        as UCAN.Resolver
import qualified Fission.Web.Auth.Token.JWT.Validation            as UCAN
import qualified Fission.Web.Auth.Token.UCAN                      as UCAN

import qualified Fission.CLI.Display.Text                         as Display
import           Fission.CLI.Environment.Class
import           Fission.CLI.Key.Store                            as KeyStore
import qualified Fission.CLI.Prompt                               as CLI.Prompt

import qualified Fission.CLI.Linking.Status.Types                 as Linking

import qualified Fission.IPFS.PubSub.Publish                      as Publish
import qualified Fission.IPFS.PubSub.Session.Key.Types            as Session
import qualified Fission.IPFS.PubSub.Session.Payload              as Session

import           Data.ByteArray                                   as ByteArray

import           Fission.IPFS.PubSub.Subscription.Secure          as Secure

import           Crypto.Error
import           Crypto.Hash.Algorithms
import qualified Crypto.PubKey.RSA.OAEP                           as RSA.OAEP
import qualified Crypto.PubKey.RSA.Types                          as RSA
import           Crypto.Random.Types

import qualified RIO.ByteString.Lazy                              as Lazy
import qualified RIO.Text                                         as Text

import           Crypto.Error
import qualified Crypto.PubKey.RSA.Types                          as RSA
import           Crypto.Random.Types

import           Network.IPFS.Local.Class                         as IPFS
import qualified Network.IPFS.Process.Error                       as IPFS.Process

import qualified Network.WebSockets                               as WS

import           Fission.Prelude

import           Fission.Key.Asymmetric.Public.Types
import qualified Fission.Key.Symmetric                            as Symmetric

import           Fission.User.DID.Types

import           Fission.Security.EncryptedWith.Types

import           Fission.Authorization.Potency.Types
import           Fission.Web.Auth.Token.JWT                       as JWT
import qualified Fission.Web.Auth.Token.JWT                       as UCAN
import qualified Fission.Web.Auth.Token.JWT.Error                 as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Class        as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Error        as UCAN.Resolver
import qualified Fission.Web.Auth.Token.JWT.Validation            as UCAN
import qualified Fission.Web.Auth.Token.UCAN                      as UCAN

import qualified Fission.IPFS.PubSub.Session.Key.Types            as Session
import qualified Fission.IPFS.PubSub.Session.Payload              as Session

import qualified Fission.IPFS.PubSub.Subscription                 as Sub
import qualified Fission.IPFS.PubSub.Subscription                 as IPFS.PubSub.Subscription
import           Fission.IPFS.PubSub.Topic

import           Fission.CLI.Key.Store                            as KeyStore
import qualified Fission.CLI.Linking.PIN                          as PIN

import           Fission.CLI.IPFS.Daemon                          as IPFS.Daemon

import qualified Fission.IPFS.PubSub.Publish                      as Publish
import qualified Fission.IPFS.PubSub.Subscription.Secure          as Secure

import           Fission.IPFS.PubSub.Session.Payload              as Payload
import           Fission.Security.EncryptedWith.Types

import           Fission.Web.Auth.Token.Bearer.Types              as Bearer

listenToLinkRequests ::
  ( MonadLogger      m
  , MonadKeyStore    m ExchangeKey
  , MonadLocalIPFS   m
  , MonadIO          m
  , MonadTime        m
  , MonadBaseControl IO m -- FIXME more semantic class
  , MonadEnvironment m
  , MonadCleanup     m
  , m `Sub.SubscribesTo` DID
  , m `Sub.SubscribesTo` Session.Payload Challenge
  , m `Raises` CryptoError
  , m `Raises` IPFS.Process.Error
  , m `Raises` String
  , m `Raises` RSA.Error
  , m `Raises` Error
  , m `Raises` Linking.Status -- FIXME only the Denied makes sense
  , MonadFail m
  )
  => DID
  -> m ()
listenToLinkRequests targetDID = do
  -- FIXME If root device, plz check first
  machineSK  <- KeyStore.fetch    (Proxy @SigningKey)
  machinePK  <- KeyStore.toPublic (Proxy @SigningKey) machineSK

  let
    machineDID = DID Key (Ed25519PublicKey machinePK)
    rootDID    = machineDID -- FIXME
    proof      = undefined --FIXME

  control \runInBase ->
    -- Step 1
    WS.runClient "runfission.net" 443 ("/user/link/did:key:z" <> show rootDID) \conn -> do
      runInBase $ reattempt 100 do
        DID Key reqPK <- ensureM $ wsReceiveJSON conn

        reqExchangeKey <- case reqPK of
                            RSAPublicKey pk -> return pk
                            _               -> raise "Not an exchange key"

        sessionKey@(Session.Key aes) <- Session.Key <$> Symmetric.genAES256
        secretSessionKey <- ensureM $ RSA.OAEP.encrypt oaepParams reqExchangeKey (Lazy.toStrict $ encode sessionKey)
        wsSend conn $ decodeUtf8Lenient secretSessionKey

        let
          -- FIXME next line jus a sketch
          now = undefined
          fromEdSK = undefined
          sessionKeyProof = authenticateSessionKey reqExchangeKey fromEdSK proof sessionKey now

        wsSend conn =<< Payload.toSecure aes sessionKeyProof

        requestorDID :: DID <- reattempt 100 $ getSecure conn sessionKey


        pin <- getSecure conn sessionKey
        confirmChallenge pin >>= \case
          False -> do
            wsSend conn =<< Payload.toSecure aes Linking.Denied
            raise Linking.Denied

          True -> do
            now <- currentTime
            let delegatedUCAN = delegateAllTo requestorDID machineSK proof now
            secureUCAN <- Payload.toSecure aes delegatedUCAN
            wsSend conn secureUCAN

wsReceiveJSON :: (MonadIO m, FromJSON a) => WS.Connection -> m (Either String a)
wsReceiveJSON conn = eitherDecode <$> wsReceive conn

wsReceive :: MonadIO m => WS.Connection -> m Lazy.ByteString
wsReceive conn =
  liftIO (WS.receiveDataMessage conn) <&> \case
    WS.Text   bs _ -> bs
    WS.Binary bs   -> bs

getSecure ::
  ( MonadIO     m
  , MonadLogger m
  , MonadRaise  m
  , m `Raises` String
  , m `Raises` CryptoError
  , FromJSON a
  )
  => WS.Connection
  -> Session.Key
  -> m a
getSecure conn (Session.Key aes256) = do
  Session.Payload
    { secretMessage = secretMsg@(EncryptedPayload ciphertext)
    , iv
    } <- ensureM $ wsReceiveJSON conn

  case Symmetric.decrypt aes256 iv secretMsg of
    Left err -> do
      -- FIXME MOVE THIS PART TO the decrypt function, even it that means wrapping in m
      logDebug $ "Unable to decrypt message via AES256: " <> decodeUtf8Lenient ciphertext
      raise err

    Right clearBS ->
      case eitherDecodeStrict $ "Bearer " <> clearBS of -- FIXME total hack
        -- FIXME better "can't decode JSON" error
        Left err -> do
          logDebug $ "Unable to decode AES-decrypted message. Raw = " <> decodeUtf8Lenient clearBS
          raise err

        Right a ->
          return a

wsSend ::
  ( MonadLogger m
  , MonadIO m
  , ToJSON msg
  , Display msg
  )
  => WS.Connection
  -> msg
  -> m ()
wsSend conn msg = do
  logDebug $ "Pushing over cleartext websocket: " <> textDisplay msg
  liftIO $ WS.sendDataMessage conn (WS.Binary $ encode msg)

confirmChallenge ::
  ( MonadCleanup m
  , MonadLogger m
  , MonadIO m
  )
  => Challenge
  -> m Bool
confirmChallenge (Challenge pinTxt) =
  Display.colourized [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow] do
    UTF8.putText "⚠️⚠️ SECURITY CHECK"

    CLI.Prompt.reaskYN $ Text.intercalate " "
      [ "🔢 Confirm that the following Challenge code is from your other device:"
      , pinTxt
      , "[Y/n]"
      ]

delegateAllTo ::
     DID
  -> SecretKey SigningKey
  -> JWT.Proof
  -> UTCTime
  -> JWT
delegateAllTo toDID fromEdSK proof now = JWT {..}
  where
    sig = JWT.Signature.Ed25519 . Key.signWith fromEdSK . encodeUtf8 $ B64.URL.encodeJWT header claims

    senderDID = DID
      { publicKey = Key.Ed25519PublicKey $ Ed25519.toPublic fromEdSK
      , method    = DID.Key
      }

    claims = JWT.Claims
      { sender   = senderDID
      , receiver = toDID

      , potency  = SuperUser
      , resource = Complete
      , proof    = proof
      , facts    = []

      , nbf      = secondsToNominalDiffTime (-30)          `addUTCTime` now
      , exp      = secondsToNominalDiffTime  3_000_000_000 `addUTCTime` now
      }

    header = JWT.Header
      { typ = JWT.Typ.JWT
      , alg = Key.Ed25519
      , cty = Nothing
      , uav = Authorization.latestVersion
      }

authenticateSessionKey ::
     DID
  -> SecretKey SigningKey
  -> JWT.Proof
  -> Session.Key
  -> UTCTime
  -> JWT
authenticateSessionKey toDID fromEdSK proof sessionKey now = JWT {..}
  where
    sig = JWT.Signature.Ed25519 . Key.signWith fromEdSK . encodeUtf8 $ B64.URL.encodeJWT header claims

    senderDID = DID
      { publicKey = Key.Ed25519PublicKey $ Ed25519.toPublic fromEdSK
      , method    = DID.Key
      }

    claims = JWT.Claims
      { sender   = senderDID
      , receiver = toDID

      , potency  = SuperUser
      , resource = None
      , proof    = proof
      , facts    = [SessionKey sessionKey]

      , nbf      = secondsToNominalDiffTime (-30)          `addUTCTime` now
      , exp      = secondsToNominalDiffTime  3_000_000_000 `addUTCTime` now
      }

    header = JWT.Header
      { typ = JWT.Typ.JWT
      , alg = Key.Ed25519
      , cty = Nothing
      , uav = Authorization.latestVersion
      }

listenForRequestorExchangeDID = do
  undefined
