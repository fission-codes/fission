module Fission.Key.EncryptedWith.Types (EncryptedWith (..)) where

import qualified RIO.ByteString.Lazy         as Lazy

import           Crypto.Cipher.AES           (AES256)
import qualified Crypto.PubKey.RSA           as RSA

import           Fission.Prelude

import qualified Fission.Key.Symmetric.Types as Symmetric

newtype EncryptedWith inner cipher
  = EncryptedPayload { cipherLBS :: Lazy.ByteString } -- FIXME needs to be b64?
  deriving newtype (Eq, Show)

instance Display (EncryptedWith inner cipher) where
  textDisplay EncryptedPayload {cipherLBS} =
    decodeUtf8Lenient $ Lazy.toStrict cipherLBS

instance ToJSON (EncryptedWith inner cipher) where
  toJSON (EncryptedPayload lbs) =
    String . decodeUtf8Lenient $ Lazy.toStrict lbs

instance FromJSON (EncryptedWith inner (Symmetric.Key AES256)) where
  parseJSON = withText "EncryptedWith (Symmetric.Key AES256)" \txt -> do
    return . EncryptedPayload . Lazy.fromStrict $ encodeUtf8 txt

instance FromJSON (EncryptedWith inner RSA.PrivateKey) where
  parseJSON = withText "EncryptedWith RSA.PrivateKey" \txt ->
    return . EncryptedPayload . Lazy.fromStrict $ encodeUtf8 txt
