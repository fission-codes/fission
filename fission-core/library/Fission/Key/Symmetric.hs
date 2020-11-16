module Fission.Key.Symmetric
  ( encrypt
  , decrypt
  , genAES256
  , genIV
  -- * Reexports
  , module Fission.Key.Symmetric.Types
  ) where

import qualified RIO.ByteString.Lazy                  as Lazy

import           Crypto.Cipher.AES                    (AES256)
import           Crypto.Cipher.Types
import           Crypto.Error
import           Crypto.Hash.Algorithms
import           Crypto.Random.Types                  as Random

import           Fission.Prelude

import           Fission.Key.Symmetric.Types          as Symmetric
import           Fission.Security.EncryptedWith.Types

-- Reexport

import           Fission.Key.Symmetric.Types

encrypt ::
  ToJSON a
  => Symmetric.Key AES256
  -> IV AES256
  -> a
  -> Either CryptoError (a `EncryptedWith` AES256)
encrypt (Symmetric.Key rawKey) iv plaintext =
  case cipherInit rawKey of
    CryptoFailed err ->
      Left err

    CryptoPassed (cipher :: AES256) ->
      case aeadInit AEAD_GCM cipher iv of
        CryptoFailed err ->
          Left err

        CryptoPassed blockCipher ->
          let
            (cipherBS, _) = aeadEncrypt blockCipher . Lazy.toStrict $ encode plaintext
          in
            Right . EncryptedPayload $ Lazy.fromStrict cipherBS

decrypt ::
     Symmetric.Key AES256
  -> IV AES256
  -> a `EncryptedWith` AES256
  -> Either CryptoError ByteString
decrypt (Symmetric.Key aesKey) iv (EncryptedPayload cipherLBS) =
  case cipherInit aesKey of
    CryptoFailed err ->
      Left err

    CryptoPassed (cipher :: AES256) ->
      case aeadInit AEAD_GCM cipher iv of
        CryptoFailed err ->
          Left err

        CryptoPassed blockCipher ->
          let
            (clearBS, _) = aeadDecrypt blockCipher $ Lazy.toStrict cipherLBS
          in
            Right clearBS

-- | Generates a string of bytes (key) of a specific length for a given block cipher
genAES256 :: MonadRandom m => m (Symmetric.Key AES256)
genAES256 = Symmetric.Key <$> getRandomBytes (blockSize (undefined :: AES256)) -- FIXME or something?

-- | Generate a random initialization vector for a given block cipher
genIV :: MonadRandom m => m (Maybe (IV AES256))
genIV = do
  bytes <- Random.getRandomBytes $ blockSize (undefined :: AES256)
  return $ makeIV (bytes :: ByteString)
