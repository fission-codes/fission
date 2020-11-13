module Fission.CLI.Linking where

import qualified System.Console.ANSI                       as ANSI

import           Crypto.Cipher.AES                         (AES256)
import           Crypto.Cipher.Types
import           Crypto.Error                              (CryptoError (..),
                                                            CryptoFailable (..))
import           Crypto.Hash.Algorithms
import qualified Crypto.PubKey.RSA.OAEP                    as RSA.OAEP
import qualified Crypto.PubKey.RSA.Types                   as RSA
import           Crypto.Random.Types                       as CRT

import           Data.ByteArray                            as ByteArray

import qualified RIO.ByteString                            as BS
import qualified RIO.ByteString.Lazy                       as Lazy
import qualified RIO.Text                                  as Text

import           Network.IPFS.Local.Class                  as IPFS
import qualified Network.IPFS.Process.Error                as IPFS.Process

import           Fission.Prelude

import qualified Fission.Internal.UTF8                     as UTF8

import           Fission.Challenge.Types
import           Fission.Security.EncryptedWith.Types

import           Fission.IPFS.PubSub.Topic
import           Fission.User.DID.Types                    as DID

import           Fission.Key.Asymmetric.Public.Types
import qualified Fission.Key.Symmetric                     as Symmetric

import qualified Fission.IPFS.PubSub.Subscription          as IPFS.PubSub.Subscription
import qualified Fission.IPFS.PubSub.Subscription          as Sub
import qualified Fission.IPFS.PubSub.Topic                 as IPFS.PubSub

import           Fission.Authorization.Potency.Types
import           Fission.Web.Auth.Token.JWT                as JWT
import qualified Fission.Web.Auth.Token.JWT                as UCAN
import qualified Fission.Web.Auth.Token.JWT.Error          as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Class as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Error as UCAN.Resolver
import qualified Fission.Web.Auth.Token.JWT.Validation     as UCAN
import qualified Fission.Web.Auth.Token.UCAN               as UCAN

import qualified Fission.CLI.Display.Text                  as Display
import           Fission.CLI.Environment.Class
import           Fission.CLI.Key.Store                     as KeyStore
import qualified Fission.CLI.Prompt                        as CLI.Prompt

import qualified Fission.CLI.Linking.Status.Types          as Linking

import qualified Fission.IPFS.PubSub.Publish               as Publish
import qualified Fission.IPFS.PubSub.Session.Key.Types     as Session
import qualified Fission.IPFS.PubSub.Session.Payload       as Session

import           Data.ByteArray                            as ByteArray

import           Fission.IPFS.PubSub.Subscription.Secure   as Secure

import           Crypto.Error
import           Crypto.Hash.Algorithms
import qualified Crypto.PubKey.RSA.OAEP                    as RSA.OAEP
import qualified Crypto.PubKey.RSA.Types                   as RSA
import           Crypto.Random.Types

import qualified RIO.ByteString.Lazy                       as Lazy
import qualified RIO.Text                                  as Text

import           Crypto.Error
import qualified Crypto.PubKey.RSA.Types                   as RSA
import           Crypto.Random.Types

import           Network.IPFS.Local.Class                  as IPFS
import qualified Network.IPFS.Process.Error                as IPFS.Process

import qualified Network.WebSockets                        as WS

import           Fission.Prelude

import           Fission.Key.Asymmetric.Public.Types
import qualified Fission.Key.Symmetric                     as Symmetric

import           Fission.User.DID.Types

import           Fission.Security.EncryptedWith.Types

import           Fission.Authorization.Potency.Types
import           Fission.Web.Auth.Token.JWT                as JWT
import qualified Fission.Web.Auth.Token.JWT                as UCAN
import qualified Fission.Web.Auth.Token.JWT.Error          as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Class as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Error as UCAN.Resolver
import qualified Fission.Web.Auth.Token.JWT.Validation     as UCAN
import qualified Fission.Web.Auth.Token.UCAN               as UCAN

import qualified Fission.IPFS.PubSub.Session.Key.Types     as Session
import qualified Fission.IPFS.PubSub.Session.Payload       as Session

import qualified Fission.IPFS.PubSub.Subscription          as Sub
import qualified Fission.IPFS.PubSub.Subscription          as IPFS.PubSub.Subscription
import           Fission.IPFS.PubSub.Topic

import           Fission.CLI.Key.Store                     as KeyStore
import qualified Fission.CLI.Linking.PIN                   as PIN

import           Fission.CLI.IPFS.Daemon                   as IPFS.Daemon

import qualified Fission.IPFS.PubSub.Publish               as Publish
import qualified Fission.IPFS.PubSub.Subscription.Secure   as Secure

import           Fission.IPFS.PubSub.Session.Payload       as Payload
import           Fission.Security.EncryptedWith.Types

import           Fission.Web.Auth.Token.Bearer.Types       as Bearer

listenToLinkRequests ::
  ( MonadLogger      m
  , MonadKeyStore    m ExchangeKey
  , MonadLocalIPFS   m
  , MonadIO          m
  , MonadBaseControl IO          m
  , MonadEnvironment m
  , MonadCleanup     m
  , m `Sub.SubscribesTo` DID
  , m `Sub.SubscribesTo` Session.Payload Challenge
  , m `Raises` CryptoError
  , m `Raises` IPFS.Process.Error
  , m `Raises` String
  , m `Raises` RSA.Error
  , m `Raises` Error
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

        requestorDID :: DID <- awaitSecure conn sessionKey -- FIXME retry

        wsSend conn =<< Payload.toSecure aes (undefined :: UCAN.JWT) -- FIXME UCAN minus potency

        pin <- awaitSecure conn sessionKey
        confirmChallenge pin >>= \case
          False ->
            wsSend conn =<< Payload.toSecure aes Linking.Denied

          True -> do
            confirmUCANDelegation requestorDID

            delegatedUCAN :: UCAN.JWT <- delegateAllTo requestorDID
            wsSend conn =<< Payload.toSecure aes delegatedUCAN

wsReceiveJSON :: (MonadIO m, FromJSON a) => WS.Connection -> m (Either String a)
wsReceiveJSON conn = eitherDecode <$> wsReceive conn

wsReceive :: MonadIO m => WS.Connection -> m Lazy.ByteString
wsReceive conn =
  liftIO (WS.receiveDataMessage conn) <&> \case
    WS.Text   bs _ -> bs
    WS.Binary bs   -> bs


awaitSecure ::
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
awaitSecure conn (Session.Key aes256) = do
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

-- {linkStatus: DENIED}
confirmUCANDelegation = undefined

delegateAllTo did = do
  undefined

listenForRequestorExchangeDID = do
  undefined
