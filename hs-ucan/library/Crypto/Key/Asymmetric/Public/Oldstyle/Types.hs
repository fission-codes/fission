module Crypto.Key.Asymmetric.Public.Oldstyle.Types (Oldstyle (..)) where

import           Control.Lens                                     ((?~))
import           Data.Aeson
import           Data.Swagger

import           RIO
import qualified RIO.Text                                         as Text

import           Servant.API

import           Crypto.Key.Asymmetric.Public.Types

newtype Oldstyle = Oldstyle { key :: Public }
  deriving stock (Eq)


-- TEXT & DISPLAY

instance Display Oldstyle where
  textDisplay = textDisplay . key

instance Show Oldstyle where
  show = show . key

instance IsString (Either Text Oldstyle) where
  fromString = parseUrlPiece . Text.pack


-- HTTP

instance ToHttpApiData Oldstyle where
  toUrlPiece = textDisplay . key

instance FromHttpApiData Oldstyle where
  parseUrlPiece txt =
    case fromJSON (String txt) :: Result Public of
      Success key -> Right (Oldstyle {..})
      Error err   -> Left (Text.pack err)


-- JSON

instance FromJSON Oldstyle where
  parseJSON = withText "Oldstyle Public Key" \txt ->
    case parseUrlPiece txt of
      Right pk -> return pk
      Left msg -> fail $ Text.unpack msg

instance ToJSON Oldstyle where
  toJSON = String . textDisplay . key


-- SWAGGER

instance ToSchema Oldstyle where
  declareNamedSchema _ =
    mempty
      & type_ ?~ SwaggerString
      & description ?~ "A public key"
      & NamedSchema (Just "PublicKey")
      & pure