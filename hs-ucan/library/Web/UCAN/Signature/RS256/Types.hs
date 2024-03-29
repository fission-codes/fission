module Web.UCAN.Signature.RS256.Types (Signature (..)) where

import           Data.ByteArray
import qualified Data.ByteString.Base64.URL as B64.URL

import           Data.Aeson
import           RIO                        hiding (length)
import           Test.QuickCheck
import           Test.QuickCheck.Instances  ()

newtype Signature = Signature { unSignature :: ByteString }
  deriving (Eq, Show)

instance Arbitrary Signature where
  arbitrary = Signature <$> arbitrary

instance FromJSON Signature where
  parseJSON = withText "RS256.Signature" \txt ->
    case B64.URL.decodeUnpadded $ encodeUtf8 txt of
      Right sig -> return $ Signature sig
      Left  err -> fail $ "Unable to parse RS256 Signature: " <> err

instance ToJSON Signature where
  toJSON = String . textDisplay

instance Display Signature where
  textDisplay (Signature raw) = decodeUtf8Lenient $ B64.URL.encodeUnpadded raw

instance ByteArrayAccess Signature where
  length        = length        . unSignature
  withByteArray = withByteArray . unSignature
