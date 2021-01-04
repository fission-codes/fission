-- | Authorization types; primarily more semantic aliases
module Fission.Web.Auth.Token.Basic.Types (Token (..)) where

import qualified RIO.Text as Text
import           Servant.API

import           Fission.Prelude

newtype Token = Token { unToken :: ByteString }
  deriving (Show, Eq)

instance Arbitrary Token where
  arbitrary = Token . encodeUtf8 <$> arbitrary

instance Display Token where
  textDisplay (Token raw) = "Basic " <> decodeUtf8Lenient raw

instance ToJSON Token where
  toJSON = String . textDisplay

instance FromJSON Token where
  parseJSON = withText "Basic Token" \txt ->
    either (fail . Text.unpack) pure $ parseUrlPiece txt

instance ToHttpApiData Token where
  toUrlPiece = textDisplay

instance FromHttpApiData Token  where
  parseUrlPiece txt =
    case Text.stripPrefix "Basic " txt <|> Text.stripPrefix "basic " txt of
      Just basic -> Right . Token $ encodeUtf8 basic
      Nothing    -> Left $ txt <> " is missing the 'Basic' prefix"
