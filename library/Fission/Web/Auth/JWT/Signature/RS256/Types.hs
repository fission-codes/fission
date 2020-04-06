module Fission.Web.Auth.JWT.Signature.RS256.Types (Signature (..)) where

import Data.ByteArray

import Fission.Prelude hiding (length)

newtype Signature = Signature { unSignature :: ByteString }
  deriving (Eq, Show)

instance Arbitrary Signature where
  arbitrary = Signature <$> arbitrary

instance FromJSON Signature where
  parseJSON = withText "RS256Siganture" \txt ->
    return . Signature $ encodeUtf8 txt

instance ToJSON Signature where
  toJSON = String . decodeUtf8Lenient . unSignature

instance ByteArrayAccess Signature where
  length        = length        . unSignature
  withByteArray = withByteArray . unSignature
