module Fission.Key.Symmetric.Types (Key (..)) where

import           Crypto.Cipher.AES (AES256)

import           Fission.Prelude

newtype Key cipher = Key { rawKey :: ByteString }
  deriving newtype Eq

instance ToJSON (Key AES256) where
  toJSON (Key bs) = String $ decodeUtf8Lenient bs

instance FromJSON (Key AES256) where
  parseJSON = withText "AES256 SymmetricKey" \txt ->
    return . Key $ encodeUtf8 txt
