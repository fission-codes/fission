module Fission.Web.Auth.Token.Types (Token (..)) where

import qualified RIO.Text as Text
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

instance Display Token where
  textDisplay (Basic  token) = textDisplay token
  textDisplay (Bearer token) = textDisplay token

instance FromJSON Token where
  parseJSON = withText "Auth.Token" \txt ->
    case parseUrlPiece txt of
      Right token -> return token
      Left  err   -> fail $ Text.unpack err

instance ToJSON Token where
  toJSON (Basic basic)   = toJSON basic
  toJSON (Bearer bearer) = toJSON bearer

instance ToHttpApiData Token where
  toUrlPiece = \case
    Basic basic   -> toUrlPiece basic
    Bearer bearer -> toUrlPiece bearer

instance FromHttpApiData Token where
  parseUrlPiece txt =
    case stripEitherPrefix "Bearer " "bearer " txt of
      Just _ ->
        Bearer <$> parseUrlPiece txt

      Nothing ->
        case stripEitherPrefix "Basic " "basic " txt of
          Just _ ->
            Basic <$> parseUrlPiece txt

          Nothing ->
            Left $ txt <> " is not a valid auth header"

stripEitherPrefix :: Text -> Text -> Text -> Maybe Text
stripEitherPrefix pfxA pfxB txt =
  Text.stripPrefix pfxA txt <|> Text.stripPrefix pfxB txt
