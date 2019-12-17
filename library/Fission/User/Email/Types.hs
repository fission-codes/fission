module Fission.User.Email.Types (Email (..)) where

import Fission.Prelude

import Data.Swagger
import Servant

import qualified RIO.ByteString.Lazy as Lazy
import qualified Network.IPFS.Internal.UTF8 as UTF8

newtype Email = Email { unEmail :: Text }
  deriving          ( Eq
                    , Generic
                    , Show
                    )
  deriving anyclass ( ToSchema )

instance ToJSON Email where
  toJSON (Email email') = toJSON (String email')

instance FromJSON Email where
  parseJSON = withText "Email" \txt -> return (Email txt)

instance MimeRender PlainText Email where
  mimeRender _ = UTF8.textToLazyBS . unEmail

instance MimeUnrender PlainText Email where
  mimeUnrender _proxy bs =
    bs
      |> Lazy.toStrict
      |> decodeUtf8'
      |> \case
          Left err  -> Left  <| show err
          Right txt -> Right <| Email txt
