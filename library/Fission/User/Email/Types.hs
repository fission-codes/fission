module Fission.User.Email.Types (Email (..)) where

import           Database.Persist.Class
import           Database.Persist.Types
import           Database.Persist.Sql

import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import qualified Network.IPFS.Internal.UTF8 as UTF8

import           Data.Swagger
import           Servant

import           Fission.Prelude

newtype Email = Email { unEmail :: Text }
  deriving          ( Generic )
  deriving anyclass ( ToSchema
                    , ToParamSchema
                    )
  deriving newtype  ( Eq
                    , Show
                    , IsString
                    , ToHttpApiData
                    )

instance Arbitrary Email where
  arbitrary = Email <$> arbitrary

instance ToJSON Email where
  toJSON (Email email') = String email'

instance FromJSON Email where
  parseJSON = withText "Email" \txt -> return (Email txt)

instance FromHttpApiData Email where
  parseUrlPiece = Right . Email

instance PersistField Email where
  toPersistValue (Email email') = PersistText email'
  fromPersistValue = \case
    PersistText email' -> Right (Email email')
    other              -> Left ("Invalid Persistent Email: " <> Text.pack (show other))

instance PersistFieldSql Email where
  sqlType _pxy = SqlString

instance MimeRender PlainText Email where
  mimeRender _ = UTF8.textToLazyBS . unEmail

instance MimeUnrender PlainText Email where
  mimeUnrender _proxy bs =
    bs
      |> Lazy.toStrict
      |> decodeUtf8'
      |> bimap show Email
