module Fission.Web.Auth.JWT.Signature.RS256.Types (Signature (..)) where

import Data.ByteArray

import Fission.Prelude hiding (length)
 
import qualified Data.ByteString.Base64 as Base64
 

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519 as Ed25519

import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Base64 as Base64

import qualified RIO.ByteString as BS
import qualified RIO.Text as Text

import RIO.Char
import qualified Data.ByteArray         as BA

import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base64.URL as B64URL

import qualified RIO.Text.Partial as PText

-- import Fission.Web.Auth.JWT.Signature.RS256 as Sig


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
 
fromURLEncoding = PText.replace "-" "+" . PText.replace "_" "/"
