module Fission.Web.Auth.JWT.Signature.RS256.Types (Signature (..)) where

import           Data.ByteArray
import qualified Data.ByteString.Base64 as Base64
import qualified RIO.Text.Partial       as PText

import           Fission.Prelude hiding (length)

newtype Signature = Signature { unSignature :: ByteString }
  deriving (Eq, Show)

instance Arbitrary Signature where
  arbitrary = Signature <$> arbitrary

instance FromJSON Signature where
  parseJSON = withText "RS256Siganture" \txt ->
    return . Signature . Base64.decodeLenient . encodeUtf8 $ fromURLEncoding txt

instance ToJSON Signature where
  toJSON = String . decodeUtf8Lenient . unSignature

instance ByteArrayAccess Signature where
  length        = length        . unSignature
  withByteArray = withByteArray . unSignature

  -- FIXME move
fromURLEncoding :: Text -> Text
fromURLEncoding = PText.replace "-" "+" . PText.replace "_" "/"
