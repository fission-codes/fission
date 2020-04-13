-- | Authorization types; primarily more semantic aliases
module Fission.Web.Auth.Token.Bearer.Types (Token (..)) where

import Data.Aeson.Types

import qualified RIO.Text as Text

import           Fission.Prelude
import           Fission.Web.Auth.JWT.Types

newtype Token = Token { unToken :: JWT }
  deriving (Show, Eq)

instance Arbitrary Token where
  arbitrary = pure . Token =<< arbitrary

instance ToJSON Token where
  toJSON (Token bs) =
    case toJSON bs of
      String txt -> String $ "Bearer " <> txt
      _          -> error "impossible"

instance FromJSON Token where
  parseJSON = withText "Bearer Token" \txt ->
    Token <$> case Text.stripPrefix "Bearer " txt of
      Just rawToken ->
        parseJSON (String rawToken) :: Parser JWT
       
      Nothing ->
        case Text.stripPrefix "bearer " txt of -- Postel's Law
          Just rawToken ->
            parseJSON (String rawToken) :: Parser JWT

          Nothing ->
            fail $ show txt <> " is missing the `Bearer ` prefix"
