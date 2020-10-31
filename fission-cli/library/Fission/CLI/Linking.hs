module Fission.CLI.Linking where

{-
1. Everyone subscribes to channel
2. Requestor broadcasts public key
3. Open a secure channel
4. Provider authentication over UCAN
5. Confirm requestor PIN
6. Credential delegation
-}

import qualified Data.ByteArray                   as ByteArray

import           Crypto.Cipher.Types
import           Crypto.Hash.Algorithms
import qualified Crypto.PubKey.RSA                as Crypto.RSA
import           Crypto.Random.Types

import qualified RIO.ByteString                   as BS

import           Fission.User.DID.Types           as DID

import qualified Data.ByteString.Lazy.Char8       as BS8
import qualified RIO.ByteString.Lazy              as Lazy
import qualified RIO.Text                         as Text

import qualified Crypto.PubKey.Ed25519            as Ed25519
import qualified Crypto.PubKey.RSA.OAEP           as RSA.OAEP
import qualified Crypto.PubKey.RSA.Types          as RSA

import           Network.IPFS.Local.Class         as IPFS
import qualified Network.IPFS.Process.Error       as IPFS.Process

import           Fission.Prelude

import qualified Fission.IPFS.PubSub.Subscription as IPFS.PubSub.Subscription
import qualified Fission.IPFS.PubSub.Subscription as Sub
import qualified Fission.IPFS.PubSub.Topic        as IPFS.PubSub

import           Fission.CLI.Key.Store            as KeyStore



import           Fission.IPFS.PubSub.Topic


import qualified Fission.Web.Auth.Token.JWT       as UCAN




import           Crypto.Cipher.AES                (AES256)
import           Crypto.Cipher.Types              (AEAD (..), AEADMode (..),
                                                   BlockCipher (..),
                                                   Cipher (..), IV,
                                                   KeySizeSpecifier (..),
                                                   makeIV, nullIV)
import           Crypto.Error                     (CryptoError (..),
                                                   CryptoFailable (..))

import qualified Crypto.Random.Types              as CRT


import           Data.ByteArray                   (ByteArray)
import           Data.ByteString                  (ByteString)

import           Fission.CLI.Environment.Class

listenToLinkRequests ::
  ( MonadLogger m
  , MonadKeyStore m ExchangeKey
  , MonadLocalIPFS m
  , MonadIO m
  , MonadRandom m
  , MonadRescue m
  , m `Sub.SubscribesTo` EncryptedWith AES256
  , m `Sub.SubscribesTo` EncryptedWith RSA.PrivateKey
  , m `Raises` CryptoError
  , m `Raises` IPFS.Process.Error
  , m `Raises` String
  , m `Raises` RSA.Error
  )
  => m ()
listenToLinkRequests = undefined

-- NOTE MonadSTM from the other branch would be nice here
requestFrom ::
  ( MonadLogger m
  , MonadKeyStore m ExchangeKey
  , MonadLocalIPFS m
  , MonadIO m
  , MonadRandom m
  , MonadRescue m
  , m `Sub.SubscribesTo` EncryptedWith AES256
  , m `Sub.SubscribesTo` EncryptedWith RSA.PrivateKey
  , m `Raises` CryptoError
  , m `Raises` IPFS.Process.Error
  , m `Raises` String
  , m `Raises` RSA.Error
  )
  => DID
  -> m ()
requestFrom targetDID =
  reattempt 10 do
    throwawaySK <- KeyStore.generate (Proxy @ExchangeKey)
    throwawayPK <- KeyStore.toPublic (Proxy @ExchangeKey) throwawaySK

    pubSubSendClear topic throwawayPK -- STEP 2, yes out of order is actually correct
    sessionKey <- getAuthenticatedSessionKey topic throwawaySK -- STEP 1-4
    secureSendPIN topic sessionKey -- STEP 5

    ucan <- listenForFinalUCAN topic sessionKey -- STEP 6
    storeUCAN ucan
  where
    topic :: IPFS.PubSub.Topic
    topic = IPFS.PubSub.Topic ("deviceLinking@" <> textDisplay targetDID)

storeUCAN = undefined

listenForFinalUCAN ::
  ( MonadIO m
  , m `Sub.SubscribesTo` EncryptedWith AES256 -- NOTE SubscribesToChannel & SubscribesToSecure
  )
  => Topic
  -> SessionKey
  -> m UCAN.JWT -- FIXME Or the raw bytestirng version? At minimum want to validate internally
listenForFinalUCAN topic aesKey =
  IPFS.PubSub.Subscription.withQueue topic \(tq :: TQueue (Sub.Message (EncryptedWith AES256))) -> do
    undefined

broadcastPK ::
  ( MonadLocalIPFS m
  , MonadRaise m
  , m `Raises` IPFS.Process.Error
  )
  => Topic
  -> RSA.PublicKey
  -> m ()
broadcastPK topic pk = pubSubSendClear topic pk -- FIXME make DID

getAuthenticatedSessionKey ::
  ( MonadIO     m
  , MonadRandom m
  , MonadRaise  m
  , m `Sub.SubscribesTo` EncryptedWith RSA.PrivateKey -- NOTE SubscribesToChannel & SubscribesToSecure
  , m `Sub.SubscribesTo` EncryptedWith AES256
  , m `Raises` RSA.Error
  , m `Raises` String
  )
  => Topic
  -> RSA.PrivateKey
  -> m SessionKey
getAuthenticatedSessionKey topic sk = do
  -- STEP 3
  sessionKey <- IPFS.PubSub.Subscription.withQueue topic $ listenForSessionKey sk

  -- STEP 4
  IPFS.PubSub.Subscription.withQueue topic $ listenForValidProof sessionKey

  return sessionKey

-- STEP 3
listenForSessionKey ::
  ( MonadIO     m
  , MonadRandom m
  , MonadRaise  m
  , m `Raises` RSA.Error
  , m `Raises` String -- FIXME better error
  )
  => RSA.PrivateKey
  -> TQueue (Sub.Message (EncryptedWith RSA.PrivateKey))
  -> m SessionKey
listenForSessionKey throwawaySK tq = readRSA throwawaySK tq

listenForValidProof ::
     SessionKey
  -> TQueue (Sub.Message (EncryptedWith AES256))
  -> m UCAN.JWT
listenForValidProof sessionKey tq = undefined -- readAES256 sessionKey tq

-- STEP 5
secureSendPIN ::
  ( MonadIO        m
  , MonadLocalIPFS m
  , MonadRandom    m
  , MonadRaise     m
  , m `Raises` IPFS.Process.Error
  , m `Raises` CryptoError
  )
  => Topic
  -> SessionKey
  -> m ()
secureSendPIN topic sessionKey = do
  randomBS <- liftIO $ getRandomBytes 6

  let
    pin :: Text
    pin = Text.takeEnd 6 . Text.pack . mconcat $ show <$> BS.unpack randomBS -- FIXME check mod direction

  pubSubSendSecure topic sessionKey pin

pubSubSendClear ::
  ( ToJSON  msg
  -- , Display msg
  , MonadLocalIPFS m
  , MonadRaise     m
  , m `Raises` IPFS.Process.Error
  )
  => Topic
  -> msg
  -> m ()
pubSubSendClear topic msg =
  void . ensureM $ IPFS.runLocal ["pubsub", "pub", Text.unpack $ textDisplay topic] (encode msg)

pubSubSendSecure ::
  ( ToJSON a
  , MonadLocalIPFS m
  , MonadRandom m
  , MonadRaise     m
  , m `Raises` IPFS.Process.Error
  , m `Raises` CryptoError
  )
  => Topic
  -> SessionKey
  -> a
  -> m ()
pubSubSendSecure topic (SessionKey aesKey) msg = do
  encrypted <- toSecureMessage aesKey msg
  pubSubSendClear topic encrypted

toSecureMessage ::
  ( MonadRandom m
  , MonadRaise  m
  , m `Raises` CryptoError
  , ToJSON msg
  )
  => SymmetricKey AES256
  -> msg
  -> m SessionMessage
toSecureMessage aesKey msg = do
  genIV >>= \case
    Nothing ->
      undefined -- FIXME better error

    Just iv -> do
      payload <- ensure $ encrypt aesKey iv msg
      return $ SessionMessage {..}

data SessionMessage = SessionMessage
  { payload :: EncryptedWith AES256
  , iv      :: IV AES256
  }

instance ToJSON SessionMessage where
  toJSON SessionMessage {..} =
    object [ "payload" .= payload
           , "iv"      .= (decodeUtf8Lenient $ ByteArray.convert iv)
           ]

instance FromJSON SessionMessage where
  parseJSON = withObject "SessionMessage" \obj -> do
    payload <- obj .: "payload"
    ivTxt   <- obj .: "iv"
    case makeIV $ encodeUtf8 ivTxt of
      Nothing -> fail "Invalid (IV AES256)"
      Just iv -> return SessionMessage {..}

newtype EncryptedWith cipher
  = EncryptedPayload { ciphertext :: ByteString }
  deriving Eq

instance ToJSON (EncryptedWith cipher) where
  toJSON (EncryptedPayload bs) = String $ decodeUtf8Lenient bs

instance FromJSON (EncryptedWith AES256) where
  parseJSON = withText "EncryptedWith AES256" \txt ->
    return . EncryptedPayload $ encodeUtf8 txt

---

newtype SymmetricKey cipher
  = SymmetricKey { rawKey :: ByteString }
  deriving newtype Eq

instance ToJSON (SymmetricKey AES256) where
  toJSON (SymmetricKey bs) = String $ decodeUtf8Lenient bs

instance FromJSON (SymmetricKey AES256) where
  parseJSON = withText "AES256 SymmetricKey" \txt ->
    return . SymmetricKey $ encodeUtf8 txt

---

newtype SessionKey = SessionKey
  { sessionKey :: SymmetricKey AES256 }
  deriving newtype Eq

instance ToJSON SessionKey where
  toJSON SessionKey {..} = object ["sessionKey" .= sessionKey]

instance FromJSON SessionKey where
  parseJSON = withObject "SessionKey" \obj -> do
    sessionKey <- obj .: "sessionKey"
    return SessionKey {..}

readRSA ::
  ( MonadIO     m
  , MonadRandom m
  , MonadRaise  m
  , m `Raises` String
  , m `Raises` RSA.Error
  , FromJSON a
  )
  => RSA.PrivateKey
  -> TQueue (Sub.Message (EncryptedWith RSA.PrivateKey))
  -- ^^^^^^^^^^^^^^^^^ FIXME maybe do this step in the queue handler?
  -> m a
readRSA sk tq = do
  -- FIXME maybe just ignore bad messags rather htan blowing up? Or retry?
  Sub.Message
    { payload = EncryptedPayload secretMsg
    } <- liftIO . atomically $ readTQueue tq

  clearBS <- ensureM $ RSA.OAEP.decryptSafer oaepParams sk secretMsg
  ensure $ eitherDecodeStrict clearBS -- FIXME better "can't decode JSON" error
  where
    oaepParams = RSA.OAEP.defaultOAEPParams SHA256

encrypt ::
  ToJSON a
  => SymmetricKey AES256
  -> IV AES256
  -> a
  -> Either CryptoError (EncryptedWith AES256)
encrypt (SymmetricKey rawKey) iv plaintext =
  case cipherInit rawKey of
    CryptoFailed err ->
      Left err

    CryptoPassed (cipher :: AES256) ->
      case aeadInit AEAD_GCM cipher iv of
        CryptoFailed err ->
          Left err

        CryptoPassed blockCipher ->
          let
            (ciphertext, _) = aeadEncrypt blockCipher . Lazy.toStrict $ encode plaintext
          in
            Right $ EncryptedPayload ciphertext

decrypt ::
     SymmetricKey AES256
  -> IV AES256
  -> EncryptedWith AES256
  -> Either CryptoError ByteString
decrypt (SymmetricKey aesKey) iv (EncryptedPayload ciphertext) =
  case cipherInit aesKey of
    CryptoFailed err ->
      Left err

    CryptoPassed (cipher :: AES256) ->
      case aeadInit AEAD_GCM cipher iv of
        CryptoFailed err ->
          Left err

        CryptoPassed blockCipher ->
          let
            (plaintext, _) = aeadDecrypt blockCipher ciphertext
          in
            Right plaintext

-- | Generates a string of bytes (key) of a specific length for a given block cipher
genAES256 :: MonadRandom m => m (SymmetricKey AES256)
genAES256 = SymmetricKey <$> getRandomBytes (blockSize (undefined :: AES256)) -- FIXME or something?

-- | Generate a random initialization vector for a given block cipher
genIV :: MonadRandom m => m (Maybe (IV AES256))
genIV = do
  bytes <- CRT.getRandomBytes $ blockSize (undefined :: AES256)
  return $ makeIV (bytes :: ByteString)
