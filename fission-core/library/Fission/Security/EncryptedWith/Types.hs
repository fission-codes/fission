module Fission.Security.EncryptedWith.Types (EncryptedWith (..)) where

import           Crypto.Cipher.AES (AES256)
import qualified Crypto.PubKey.RSA as RSA

import           Fission.Prelude

newtype EncryptedWith inner cipher
  = EncryptedPayload { ciphertext :: ByteString }
  deriving newtype (Eq, Show)

instance Display (EncryptedWith inner cipher) where
  textDisplay EncryptedPayload {ciphertext} =
    decodeUtf8Lenient ciphertext

instance ToJSON (EncryptedWith inner cipher) where
  toJSON (EncryptedPayload bs) = String $ decodeUtf8Lenient bs

instance FromJSON (EncryptedWith inner AES256) where
  parseJSON = withText "EncryptedWith AES256" \txt -> do
    return . EncryptedPayload $ encodeUtf8 txt

instance FromJSON (EncryptedWith inner RSA.PrivateKey) where
  parseJSON = withText "EncryptedWith RSA.PrivateKey" \txt ->
    return . EncryptedPayload $ encodeUtf8 txt
