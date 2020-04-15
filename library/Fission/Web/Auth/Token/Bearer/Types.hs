-- | Authorization types; primarily more semantic aliases
module Fission.Web.Auth.Token.Bearer.Types (Token (..)) where

import           Data.Aeson.Types
import           Servant.API

import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import           Fission.Prelude
import           Fission.Web.Auth.JWT.Types

data Token = Token
  { jwt        :: !JWT
  , rawContent :: !(Maybe ByteString) -- To pass in to the verifier
  }
  deriving (Show, Eq)

instance Arbitrary Token where
  arbitrary = do
    let rawContent = Nothing
    jwt <- arbitrary
    return Token {..}

instance ToJSON Token where
  toJSON (Token bs _) =
    case toJSON bs of
      String txt -> String $ "Bearer " <> txt
      _          -> error "impossible"

instance FromJSON Token where
  parseJSON = withText "Bearer Token" \txt ->
    case Text.stripPrefix "Bearer " txt of
      Just rawToken ->
        resolve rawToken

      Nothing ->
        case Text.stripPrefix "bearer " txt of -- Postel's Law
          Just rawToken ->
            resolve rawToken
           
          Nothing ->
            fail $ show txt <> " is missing the `Bearer ` prefix"

    where
      justContent =
        Just . encodeUtf8 . Text.dropEnd 1 . Text.dropWhileEnd (/= '.')
       
      resolve rawToken = do
        jwt <- parseJSON (String rawToken)
        let rawContent = justContent rawToken
        return Token {..}

instance ToHttpApiData Token where
  toUrlPiece Token {jwt} =
    Text.dropEnd 1 . Text.drop 1 . decodeUtf8Lenient . Lazy.toStrict $ encode jwt

instance FromHttpApiData Token  where
  parseUrlPiece txt =
    case eitherDecode . Lazy.fromStrict $ encodeUtf8 ("\"" <> txt <> "\"") of
      Right token -> Right token
      Left  err   -> Left $ Text.pack err
