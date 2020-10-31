module Fission.CLI.Linking where

{-
1. Everyone subscribes to channel
2. Requestor broadcasts public key
3. Open a secure channel
4. Provider authentication over UCAN
5. Confirm requestor PIN
6. Credential delegation
-}

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


-- NOTE MonadSTM from the other branch would be nice here
requestFrom ::
  ( MonadLogger m
  , MonadKeyStore m ExchangeKey
  , MonadLocalIPFS m
  , MonadIO m
  , MonadRandom m
  , MonadRescue m
  , m `Sub.SubscribesTo` Sub.Message ByteString
  , m `Sub.SubscribesTo` Sub.Message ByteString -- Make work with AES and RSA stuiff
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

    ucan <- listenForFinalUCAN sessionKey -- STEP 6
    storeUCAN ucan
  where
    topic :: IPFS.PubSub.Topic
    topic = IPFS.PubSub.Topic ("deviceLinking@" <> textDisplay targetDID)

storeUCAN = undefined

lisenForFinalUCAN =
  IPFS.PubSub.Subscription.withQueue topic \tq -> do
    undefined

broadcasePK = pubSubSend topic throwawayPK -- FIXME make DID

getAuthenticatedSessionKey topic sk =
  IPFS.PubSub.Subscription.withQueue topic \tq -> do
    -- STEP 3
    sessionKey <- listenForSessionKey sk tq

    -- STEP 4
    listenForValidProof sessionKey

    return sessionKey

-- STEP 3
listenForSessionKey :: m (SymmetricKey AES256 ByteString)
listenForSessionKey throwawaySK tq = ensureM $ readRSA throwawaySK tq

listenForValidProof sessionKey tq = undefined -- readAES256 sessionKey tq

-- STEP 5
secureSendPIN topic sessionKey = do
  randomBS <- liftIO $ getRandomBytes 6

  let
    pin :: Text
    pin = mconact $ (textDisplay . mod 10) <$> BS.unpack randomBS -- FIXME check mod direction

  pubsubSend topic (encrypt pin)

pubSubSendClear ::
  ( ToJSON msg
  , MonadRaise m
  , m `Raises` IPFS.Process.Error
  )
  => Topic
  -> msg
  -> m ()
pubSubSendClear topic msg =
  ensureM $ IPFS.runLocal
    ["pubsub", "pub", Text.unpack $ textDisplay topic]
    (Lazy.fromStrict . encodeUtf8 $ textDisplay msg)

pubSubSendSecure topic msg aesKey =
  pubSubSendClear topic (msg `encryptWith` aesKey)

-- class Encryptable cipher a m where
--   data Encrypted cipher
--
--   data Encryptor cipher
--   data Decryptor cipher
--
--   encryptWith ::
--        a
--     -> Encryptor cipher
--     -> m (CryptoFailable EncryptedPayload)
--
--   decryptWith ::
--        Encrypted cipher a
--     -> Decryptor cipher
--     -> m (CryptoFailable a)


--
--   encryptWith payload aesKey =
--     genIV >>= \case
--       Nothing ->
--         undefined -- FIXME CryptoError? Maybe put on genIV
--
--       Just iv ->
--         encrypt aesKey iv payload


-- instance Encryptable RSA2048  -- FIXME or somehting





-- newtype Ciphertext cipher a = Ciphertext ByteString
--
-- instance Encryptable cipher a => Encryptable (Ciphertext cipher a) where
--   encryptWith _ (Ciphertext a) = encryptWith (Proxy @cipher) a
--
-- instance (ToJSON a, Encryptable a) => ToJSON (Ciphertext AES256 a) where
--   toJSON (Ciphertext a) = String . decodeUtf8Lenient $ encryptWith AES256 a
--
-- instance FromJSON a => FromJSON (Ciphertext AES256 a) where
--   parseJSON raw = Ciphertext <$> parseJSON raw

-- data EncryptedPayload cipher a = EncryptedPayload
--   { publicData :: PublicData cipher -- ^ e.g. IV for AES or PK for RSA
--   , cipherText :: ByteString
--   }

-- instance (ToJSON a, Encryptable a) => ToJSON (EncryptedPayload AES256 a) where
--   toJSON (Ciphertext a) = String . decodeUtf8Lenient $ encryptWith AES256 a
--
-- instance FromJSON a => FromJSON (Ciphertext AES256 a) where
--   parseJSON raw = Ciphertext <$> parseJSON raw

data SessionPayload = SessionPayload
  { encrypted :: ByteString
  , iv        :: IV AES256
  }

newtype EncryptedWith cipher
  = EncryptedPayload { getEncryptedPayload :: ByteString }
  deriving Eq

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

newtype SessionKeyExchange = SessionKeyExchange
  { sessionKey :: SymmetricKey AES256 }
  deriving newtype Eq

instance ToJSON SessionKeyExchange where
  toJSON (SessionKeyExchange key) = object ["sessionKey" .= key]

instance FromJSON SessionKeyExchange where
  parseJSON = withObject "SessionKeyExchange" \obj -> do
    sessionKey <- obj .: "sessionKey"
    return SessionKeyExchange {..}

data RSA2048

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
  Sub.Message {payload = secretMsg} <- liftIO . atomically $ readTQueue tq
  clearBS <- ensureM $ RSA.OAEP.decryptSafer oaepParams sk secretMsg
  ensure $ eitherDecodeStrict clearBS -- FIXME better "can't decode JSON" error
  where
    oaepParams = RSA.OAEP.defaultOAEPParams SHA256

encrypt ::
     SymmetricKey AES256
  -> IV  AES256
  -> ByteString
  -> CryptoFailable (AEAD AES256) -- or AEAD a?
encrypt (SymmetricKey rawKey) iv msg =
  case cipherInit rawKey of
    CryptoFailed err    ->
      CryptoFailed err

    CryptoPassed cipher ->
      let
        blockCipher = aeadInit AEAD_GCM cipher iv -- FIXME doesn't use msg?!
      in
        return $ ctrCombine blockCipher iv msg -- AFAICT ctrCombine if correct for GCM encryption

decrypt ::
  ByteArray a
  => SymmetricKey AES256
  -> IV  AES256
  -> ByteString
  -> Either CryptoError a
decrypt = encrypt -- NOTE shockingly this is correct

-- | Not required, but most general implementation
-- data SymmetricKey c a where
  -- SymmetricKey :: (BlockCipher c, ByteArray a) => a -> SymmetricKey c a



-- | Generates a string of bytes (key) of a specific length for a given block cipher
genAES256 :: (MonadRandom m, ByteArray a) => m (SymmetricKey AES256)
genAES256 = SymmetricKey <$> getRandomBytes (blockSize (undefined :: AES256)) -- FIXME or something?

-- | Generate a random initialization vector for a given block cipher
genIV :: MonadRandom m => m (Maybe (IV AES256))
genIV = do
  bytes <- CRT.getRandomBytes $ blockSize (undefined :: AES256)
  return $ makeIV (bytes :: ByteString)

-- | Initialize a block cipher
-- initCipher :: ByteArray a => SymmetricKey AES256 a -> Either CryptoError AES256
-- initCipher (SymmetricKey k) = -- NOTE just use the cryptofalable version fs
--   case cipherInit k of
--     CryptoFailed e -> Left e
--     CryptoPassed a -> Right a
