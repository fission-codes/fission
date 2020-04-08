-- | Authorization types; primarily more semantic aliases
module Fission.Web.Auth.Token.Bearer.Types (Token (..)) where

import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import           Fission.Prelude
import qualified Fission.Internal.UTF8 as UTF8

import           Fission.Web.Auth.JWT.Types

newtype Token = Token { unToken :: JWT }
  deriving (Show, Eq)

instance Arbitrary Token where
  arbitrary = pure . Token =<< arbitrary

instance ToJSON Token where
  toJSON (Token bs) = String $ "Bearer " <> token
    where
      token :: Text
      token = UTF8.stripQuotes $ decodeUtf8Lenient $ Lazy.toStrict $ encode bs

instance FromJSON Token where
  parseJSON = withText "Bearer Token" \txt ->
    case Text.stripPrefix "Bearer " txt of
      Nothing ->
        fail $ show txt <> " is missing the `Bearer ` prefix"
 
      Just rawToken -> do
        rawToken
          |> UTF8.stripQuotes
          |> encodeUtf8
          |> Lazy.fromStrict
          |> eitherDecode
          |> \case
              Left err -> fail $ "Unable to parse JWT in bearer token: " <> show err
              Right jwt -> return $ Token jwt
