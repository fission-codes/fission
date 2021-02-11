module Fission.URL.Subdomain.Types (Subdomain (..)) where

import qualified RIO.ByteString.Lazy    as Lazy
import qualified RIO.Text               as Text

import           Data.Swagger
import           Database.Persist.Sql
import           Servant.API

import           Fission.Prelude

import qualified Fission.App.Name.Types as App

-- | Type safety wrapper for subdomains
newtype Subdomain = Subdomain { raw :: Text }
  deriving newtype (Eq , Show, Display)

instance Semigroup Subdomain where
  Subdomain subA <> Subdomain subB = Subdomain (subA <> "." <> subB)

instance FromJSON Subdomain where
  parseJSON = withText "AWS.Subdomain" \txt ->
    return $ Subdomain txt

instance ToJSON Subdomain where
  toJSON (Subdomain sub) = String sub

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

instance ToParamSchema Subdomain where
  toParamSchema _ = mempty |> type_ ?~ SwaggerString

instance FromHttpApiData Subdomain where
  parseUrlPiece = Right . Subdomain

instance ToHttpApiData Subdomain where
  toUrlPiece = textDisplay

instance MimeRender PlainText Subdomain where
  mimeRender _ = displayLazyBS . raw

instance MimeRender OctetStream Subdomain where
  mimeRender _ = displayLazyBS . raw

instance MimeUnrender PlainText Subdomain where
  mimeUnrender _proxy bs =
    bs
      |> Lazy.toStrict
      |> decodeUtf8'
      |> bimap show Subdomain

instance Arbitrary Subdomain where
  arbitrary = do
    appName :: App.Name <- arbitrary
    return . Subdomain $ textDisplay appName
