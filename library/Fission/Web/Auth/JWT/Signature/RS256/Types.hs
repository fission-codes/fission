module Fission.Web.Auth.JWT.Signature.RS256.Types (Signature (..)) where

import           Data.ByteArray
import qualified Data.ByteString.Base64 as B64

import           Fission.Prelude hiding (length)
import qualified Fission.Internal.Base64.URL as B64.URL

newtype Signature = Signature { unSignature :: ByteString }
  deriving (Eq, Show)

instance Arbitrary Signature where
  arbitrary = Signature <$> arbitrary

instance FromJSON Signature where
  parseJSON = withText "RS256.Siganture" \txt ->
    -- pure . Signature . encodeUtf8 $ B64.URL.decode txt
    pure . Signature . B64.decodeLenient . encodeUtf8 $ B64.URL.decode txt
    -- pure . Signature $ encodeUtf8 txt

instance ToJSON Signature where
  toJSON = String . decodeUtf8Lenient . unSignature

instance ByteArrayAccess Signature where
  length        = length        . unSignature
  withByteArray = withByteArray . unSignature
