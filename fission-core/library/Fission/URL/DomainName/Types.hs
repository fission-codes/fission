module Fission.URL.DomainName.Types (DomainName (..)) where

import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Char            as Char
import qualified RIO.Text            as Text

import           Database.Persist.Postgresql hiding (get)
import           Data.Swagger                hiding (get, host)

import           Servant

import           Fission.Prelude

-- | Type safety wrapper for domain names
newtype DomainName = DomainName { get :: Text }
  deriving          ( Eq
                    , Generic
                    , Show
                    , Read
                    , Ord
                    )
  deriving newtype  ( IsString
                    , PathPiece
                    )

instance Arbitrary DomainName where
  arbitrary = do
    host <- Text.filter Char.isAlpha <$> arbitrary
    tld  <- elements ["com", "net", "co.uk", "dev", "codes"]

    return . DomainName $ host <> "." <> tld

instance Display DomainName where
  textDisplay (DomainName txt) = txt

instance PersistField DomainName where
  toPersistValue (DomainName name') = PersistText name'
  fromPersistValue = \case
    PersistText name' -> Right (DomainName name')
    other             -> Left ("Invalid Persistent Domain Name: " <> Text.pack (show other))

instance PersistFieldSql DomainName where
  sqlType _pxy = SqlString

instance ToSchema DomainName where
  declareNamedSchema _ =
    mempty
      |> example     ?~ "myawesomedomain.com"
      |> description ?~ "A domain name"
      |> type_       ?~ SwaggerString
      |> NamedSchema (Just "DomainName")
      |> pure

instance ToParamSchema DomainName where
  toParamSchema _ = mempty |> type_ ?~ SwaggerString

instance ToHttpApiData DomainName where
  toUrlPiece (DomainName domainName) = domainName

instance FromHttpApiData DomainName where
  parseUrlPiece = Right . DomainName

instance ToJSON DomainName where
  toJSON (DomainName domainName) = String domainName

instance FromJSON DomainName where
  parseJSON = withText "AWS.DomainName" \txt ->
    DomainName <$> parseJSON (String txt)

instance MimeRender PlainText DomainName where
  mimeRender _ = displayLazyBS . get

instance MimeRender OctetStream DomainName where
  mimeRender _ = displayLazyBS . get

instance MimeUnrender PlainText DomainName where
  mimeUnrender _proxy bs =
    bs
      |> Lazy.toStrict
      |> decodeUtf8'
      |> bimap show DomainName
