module Fission.Web.Auth.Token.Types (Token (..)) where

import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import           Servant.API

import           Fission.Prelude

import qualified Fission.Web.Auth.Token.Basic.Types  as Basic
import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer

data Token
  = Basic  Basic.Token
  | Bearer Bearer.Token
  deriving (Show, Eq)

instance Arbitrary Token where
  arbitrary = oneof [Basic <$> arbitrary, Bearer <$> arbitrary]

instance FromJSON Token where
  parseJSON = withText "Auth.Token" \txt ->
    case stripEitherPrefix "Basic " "basic " txt of
      Just _ ->
        Basic <$> parseJSON (String txt)

      Nothing ->
        case stripEitherPrefix "Bearer " "bearer " txt of
          Just _ ->
            Bearer <$> parseJSON (String txt)

          Nothing ->
            fail $ show txt <> " is not a valid auth header"

instance ToJSON Token where
  toJSON (Basic basic)   = toJSON basic
  toJSON (Bearer bearer) = toJSON bearer

instance ToHttpApiData Token where
  toUrlPiece = \case
    Basic basic   -> toUrlPiece basic
    Bearer bearer -> toUrlPiece bearer

instance FromHttpApiData Token  where
  parseUrlPiece txt =
    case eitherDecode . Lazy.fromStrict $ encodeUtf8 ("\"" <> txt <> "\"") of
      Right token -> Right token
      Left  err   -> Left $ Text.pack err

stripEitherPrefix :: Text -> Text -> Text -> Maybe Text
stripEitherPrefix pfxA pfxB txt =
  case Text.stripPrefix pfxA txt of
    Just txts -> Just txts
    Nothing   -> Text.stripPrefix pfxB txt
