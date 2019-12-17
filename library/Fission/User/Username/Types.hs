module Fission.User.Username.Types (Username (..)) where

import Fission.Prelude

import Data.Swagger
import Servant

import qualified RIO.ByteString.Lazy as Lazy
import qualified Network.IPFS.Internal.UTF8 as UTF8

newtype Username = Username { username :: Text }
  deriving          ( Eq
                    , Generic
                    , Show
                    )
  deriving anyclass ( ToParamSchema )
  deriving newtype  ( IsString
                    , ToHttpApiData
                    )

instance ToJSON Username where
  toJSON (Username username) = toJSON (String username)

instance FromJSON Username where
  parseJSON = withText "Username" \txt -> return (Username txt)

instance FromHttpApiData Username where
  parseUrlPiece = Right . Username

instance ToSchema Username where
  declareNamedSchema _ =
    mempty
      |> type_   ?~ SwaggerString
      |> example ?~ "username"
      |> NamedSchema (Just "Username")
      |> pure

instance MimeRender PlainText Username where
  mimeRender _ = UTF8.textToLazyBS . username

instance MimeUnrender PlainText Username where
  mimeUnrender _proxy bs =
    case bs |> Lazy.toStrict |> decodeUtf8' of
      Left err  -> Left  <| show err
      Right txt -> Right <| Username txt
