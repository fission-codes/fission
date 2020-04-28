-- | Authorization types; primarily more semantic aliases
module Fission.Web.Auth.Token.Basic.Types (Token (..)) where

import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import           Servant.API

import           Fission.Prelude

newtype Token = Token { unToken :: ByteString }
  deriving (Show, Eq)

instance Arbitrary Token where
  arbitrary = Token . encodeUtf8 <$> arbitrary

instance Display Token where
  textDisplay = Text.pack . show

instance ToJSON Token where
  toJSON (Token bs) = String $ "Basic " <> decodeUtf8Lenient bs

instance FromJSON Token where
  parseJSON = withText "Basic Token" \txt ->
    case Text.stripPrefix "Basic " txt of
      Just basic -> pure . Token $ encodeUtf8 basic
      Nothing    -> fail $ show txt <> " is missing the 'Basic' prefix"

instance ToHttpApiData Token where
  toUrlPiece token =
    Text.dropEnd 1 . Text.drop 1 . decodeUtf8Lenient . Lazy.toStrict $ encode token

instance FromHttpApiData Token  where
  parseUrlPiece txt =
    case eitherDecodeStrict $ encodeUtf8 ("\"" <> txt <> "\"") of
      Right token -> Right token
      Left  err   -> Left $ Text.pack err
