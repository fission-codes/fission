module Fission.CLI.User.Link.Payload.Types (Payload (..)) where

import           Crypto.Cipher.AES                   (AES256)

import           Servant.API

import           Fission.Prelude

import qualified Fission.Key.Symmetric.Types         as Symmetric
import           Fission.Web.Auth.Token.Bearer.Types as Bearer

data Payload = Payload
  { readKey :: Symmetric.Key AES256
  , bearer  :: Bearer.Token
  }
  deriving Eq

instance ToJSON Payload where
  toJSON Payload {readKey, bearer = Bearer.Token {rawContent}} =
    object [ "readKey" .= readKey
           , "ucan"    .= rawContent
           ]

instance FromJSON Payload where
  parseJSON = withObject "Payload" \obj -> do
    readKey <- obj .: "readKey"
    rawUCAN <- obj .: "ucan"
    bearer  <- parseJSON $ String ("bearer " <> rawUCAN)

    return Payload {..}

instance MimeRender OctetStream Payload where
  mimeRender _ payload = encode payload

instance MimeUnrender OctetStream Payload where
  mimeUnrender _ lbs = eitherDecode lbs
