module Fission.Security.EncryptedWith.Types (EncryptedWith (..)) where

import qualified RIO.ByteString.Lazy as Lazy

import           Crypto.Cipher.AES   (AES256)
import qualified Crypto.PubKey.RSA   as RSA

import           Fission.Prelude

newtype EncryptedWith inner cipher
  = EncryptedPayload { cipherLBS :: Lazy.ByteString }
  deriving newtype (Eq, Show)

instance Display (EncryptedWith inner cipher) where
  textDisplay EncryptedPayload {cipherLBS} =
    decodeUtf8Lenient $ Lazy.toStrict cipherLBS

instance ToJSON (EncryptedWith inner cipher) where
  toJSON (EncryptedPayload lbs) =
    String . decodeUtf8Lenient $ Lazy.toStrict lbs

instance FromJSON (EncryptedWith inner AES256) where
  parseJSON = withText "EncryptedWith AES256" \txt -> do
    return . EncryptedPayload . Lazy.fromStrict $ encodeUtf8 txt

instance FromJSON (EncryptedWith inner RSA.PrivateKey) where
  parseJSON = withText "EncryptedWith RSA.PrivateKey" \txt ->
    return . EncryptedPayload . Lazy.fromStrict $ encodeUtf8 txt
