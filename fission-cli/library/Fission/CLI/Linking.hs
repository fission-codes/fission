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
import           Crypto.Random.Types

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
  -- , MonadEnvironment m
  , MonadKeyStore m ExchangeKey
  , MonadLocalIPFS m
  , MonadIO m
  , MonadRandom m
  , MonadRescue m
  , m `Sub.SubscribesTo` (Either String (Sub.Message ByteString))
  , m `Raises` CryptoError
  , m `Raises` IPFS.Process.Error
  , m `Raises` String
  , m `Raises` RSA.Error
  )
  => DID
  -> m ()
requestFrom targetDID = do
  let
    targetTxt = textDisplay targetDID
    topic     = IPFS.PubSub.Topic targetTxt

  -- STEP 2
  throwawaySK <- KeyStore.generate (Proxy @ExchangeKey)
  throwawayPK <- KeyStore.toPublic (Proxy @ExchangeKey) throwawaySK

  reattempt 10 do
    ensureM $ IPFS.runLocal
      ["pubsub", "pub", Text.unpack targetTxt]
      (Lazy.fromStrict . encodeUtf8 $ textDisplay throwawayPK)

    -- STEP 1
    IPFS.PubSub.Subscription.withQueue topic \tq -> do
      sessionKey :: CryptoKey AES256 ByteString <- reattempt 10 do
        ensureM $ readRSA throwawaySK tq
      -- let did = DID (Ed25519.toPublic sk) Key
      return sessionKey

    return ()

newtype Encrypted cipher a = Encrypted a
  deriving newtype Eq

instance ToJSON a => ToJSON (Encypted a) where
  toJSON = encode a -- FIXME BROOKE YOU'RE HERE! Working on an encypted wrapper for atoencode/decoee, becase of TQUEU types

data AESKeyExchange = AESKeyExchange
  { sessionKey :: CryptoKey AES256 ByteString }
  deriving Eq -- NOTE do not create a show instance

instance ToJSON (CryptoKey AES256 ByteString) where
  toJSON (CryptoKey bs) = String $ decodeUtf8Lenient bs

instance FromJSON (CryptoKey AES256 ByteString) where
  parseJSON = withText "AES256 CryptoKey" \txt ->
    return . CryptoKey $ encodeUtf8 txt

instance ToJSON AESKeyExchange where
  toJSON (AESKeyExchange key) =
    object ["sessionKey" .= key]

instance FromJSON AESKeyExchange where
  parseJSON = withObject "AESKeyExchange" \obj -> do
    sessionKey <- obj .: "sessionKey"
    return AESKeyExchange {..}

readRSA ::
  ( MonadIO     m
  , MonadRandom m
  , MonadRaise  m
  , m `Raises` String
  , m `Raises` RSA.Error
  , FromJSON a
  )
  => RSA.PrivateKey
  -> TQueue (Either String (Sub.Message (CryptoKey AES256 ByteString))) -- FIXME maybe do this step in the queue handler?
  -> m a
readRSA sk tq = do
  -- FIXME maybe just ignore bad messags rather htan blowing up? Or retry?
  Sub.Message {payload = secretMsg} <- ensureM . liftIO . atomically $ readTQueue tq
  clearBS <- ensureM $ RSA.OAEP.decryptSafer oaepParams sk secretMsg
  ensure $ eitherDecodeStrict clearBS -- FIXME better "can't decode" error

  where
    oaepParams = RSA.OAEP.defaultOAEPParams SHA256

encrypt ::
  ByteArray a
  => CryptoKey AES256 a
  -> IV  AES256
  -> a
  -> CryptoFailable (AEAD AES256) -- or AEAD a?
encrypt (CryptoKey rawKey) iv msg =
  case cipherInit rawKey of
    CryptoFailed err    -> CryptoFailed err
    CryptoPassed cipher -> aeadInit AEAD_GCM cipher iv

decrypt ::
  ByteArray a
  => CryptoKey AES256 a
  -> IV  AES256
  -> a
  -> Either CryptoError a
decrypt = undefined -- encrypt -- FIXME MASSIVELY RAISED EYEBROWS

-- | Not required, but most general implementation
data CryptoKey c a where
  CryptoKey :: (BlockCipher c, ByteArray a) => a -> CryptoKey c a

instance Eq (CryptoKey c a) where
  CryptoKey a == CryptoKey b = a == b

-- | Generates a string of bytes (key) of a specific length for a given block cipher
genAES256 ::
  ( MonadRandom m
  , ByteArray   a
  )
  => Proxy AES256
  -> Natural
  -> m (CryptoKey AES256 a)
genAES256 _ size = CryptoKey <$> getRandomBytes (fromIntegral size)

-- | Generate a random initialization vector for a given block cipher
genIV :: MonadRandom m => m (Maybe (IV AES256))
genIV = do
  bytes <- CRT.getRandomBytes $ blockSize (undefined :: AES256)
  return $ makeIV (bytes :: ByteString)

-- | Initialize a block cipher
-- initCipher :: ByteArray a => CryptoKey AES256 a -> Either CryptoError AES256
-- initCipher (CryptoKey k) = -- NOTE just use the cryptofalable version fs
--   case cipherInit k of
--     CryptoFailed e -> Left e
--     CryptoPassed a -> Right a
