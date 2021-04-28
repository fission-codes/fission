module Fission.Key.Symmetric
  ( encrypt
  , decrypt
  , genAES256
  , genIV
  -- * Reexports
  , module Fission.Key.Symmetric.Types
  ) where

import qualified RIO.ByteString                  as BS
import qualified RIO.ByteString.Lazy             as Lazy

import qualified Data.ByteArray                  as BA

import           Crypto.Cipher.AES               (AES256)
import           Crypto.Cipher.Types
import           Crypto.Error
import           Crypto.Random.Types             as Random

import           Servant.API

import           Fission.Prelude

import           Fission.Key.EncryptedWith.Types
import qualified Fission.Key.IV.Error            as IV
import           Fission.Key.Symmetric.Types     as Symmetric

-- Reexport

import           Fission.Key.Symmetric.Types

encrypt ::
  MimeRender OctetStream a
  => Symmetric.Key AES256
  -> IV AES256
  -> a
  -> Either CryptoError (a `EncryptedWith` Symmetric.Key AES256)
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
            (authTag, cipherBS) = aeadSimpleEncrypt blockCipher ("" :: ByteString) (Lazy.toStrict $ mimeRender (Proxy @OctetStream) plaintext) 16
          in
            Right . EncryptedPayload $ Lazy.fromStrict (cipherBS <> BA.convert authTag)

decrypt ::
     Symmetric.Key AES256
  -> IV AES256
  -> a `EncryptedWith` Symmetric.Key AES256
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
            (cipherBS, tagBS) = BS.splitAt (fromIntegral $ Lazy.length cipherLBS - 16) (Lazy.toStrict cipherLBS)
            authTag = AuthTag $ BA.convert tagBS
            mayClearBS = aeadSimpleDecrypt blockCipher ("" :: ByteString) cipherBS authTag
          in
            case mayClearBS of
              Nothing      -> Left CryptoError_MacKeyInvalid -- NOTE Fails when auth tag doens't match, hence this error
              Just clearBS -> Right clearBS

-- | Generates a string of bytes (key) of a specific length for a given block cipher
genAES256 :: MonadRandom m => m (Symmetric.Key AES256)
genAES256 = Symmetric.Key <$> getRandomBytes 32

-- | Generate a random initialization vector for a given block cipher
genIV :: MonadRandom m => m (Either IV.GenError (IV AES256))
genIV = do
  bytes <- Random.getRandomBytes $ blockSize (undefined :: AES256)
  case makeIV (bytes :: ByteString) of
    Nothing -> return $ Left IV.GenError
    Just iv -> return $ Right iv
