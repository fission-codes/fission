-- | Authorization types; primarily more semantic aliases
module Fission.Web.Auth.Token.Bearer.Types (Token (..)) where

import           Data.Aeson.Types
import           Servant.API

import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import           Fission.Prelude
import qualified Fission.Internal.Base64.URL as B64.URL
 
import           Fission.Web.Auth.Token.JWT

data Token = Token
  { jwt        :: !JWT
  , rawContent :: !(Maybe Text) -- To pass in to the verifier
  }
  deriving (Show, Eq)

instance Arbitrary Token where
  arbitrary = do
    jwt@JWT {..} <- arbitrary
    let rawContent = Just $ B64.URL.encodeJWT header claims
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
        Just . Text.dropEnd 1 . Text.dropWhileEnd (/= '.')
       
      resolve rawToken = do
        jwt <- parseJSON (String rawToken)
        let rawContent = justContent rawToken
        return Token {..}

instance ToHttpApiData Token where
  toUrlPiece token =
    Text.dropEnd 1 . Text.drop 1 . decodeUtf8Lenient . Lazy.toStrict $ encode token

instance FromHttpApiData Token  where
  parseUrlPiece txt =
    case eitherDecodeStrict' $ encodeUtf8 ("\"" <> txt <> "\"") of
      Right token -> Right token
      Left  err   -> Left $ Text.pack err
