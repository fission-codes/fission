module Fission.URL.Subdomain.Types (Subdomain (..)) where

import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import           Database.Persist.Postgresql hiding (get)
import           Data.Swagger                hiding (get)

import           Servant

import           Fission.Prelude
import qualified Fission.Internal.UTF8 as UTF8

-- | Type safety wrapper for subdomains
newtype Subdomain = Subdomain { get :: Text }
  deriving          ( Eq
                    , Generic
                    , Show
                    )
  deriving newtype  ( IsString )

instance PersistField Subdomain where
  toPersistValue (Subdomain name') = PersistText name'
  fromPersistValue = \case
    PersistText name' -> Right (Subdomain name')
    other             -> Left ("Invalid Persistent Domain Name: " <> Text.pack (show other))

instance PersistFieldSql Subdomain where
  sqlType _pxy = SqlString

instance ToSchema Subdomain where
  declareNamedSchema _ =
    mempty
      |> example     ?~ "myawesomedomain.com"
      |> description ?~ "A domain name"
      |> type_       ?~ SwaggerString
      |> NamedSchema (Just "Subdomain")
      |> pure

instance FromJSON Subdomain where
  parseJSON = withText "AWS.Subdomain" \txt ->
    Subdomain <$> parseJSON (String txt)

instance MimeRender PlainText Subdomain where
  mimeRender _ = UTF8.textToLazyBS . get

instance MimeRender OctetStream Subdomain where
  mimeRender _ = UTF8.textToLazyBS . get

instance MimeUnrender PlainText Subdomain where
  mimeUnrender _proxy bs =
    bs
      |> Lazy.toStrict
      |> decodeUtf8'
      |> bimap show Subdomain
