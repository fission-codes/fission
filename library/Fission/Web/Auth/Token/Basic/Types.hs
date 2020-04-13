-- | Authorization types; primarily more semantic aliases
module Fission.Web.Auth.Token.Basic.Types (Token (..)) where

import qualified RIO.Text as Text

import           Fission.Prelude

newtype Token = Token { unToken :: ByteString }
  deriving (Show, Eq)

instance Arbitrary Token where
  arbitrary = Token . encodeUtf8 <$> arbitrary

instance ToJSON Token where
  toJSON (Token bs) = String $ "Basic " <> decodeUtf8Lenient bs

instance FromJSON Token where
  parseJSON = withText "Basic Token" \txt ->
    case Text.stripPrefix "Basic " txt of
      Just basic -> pure . Token $ encodeUtf8 basic
      Nothing    -> fail $ show txt <> " is missing the 'Basic' prefix"
