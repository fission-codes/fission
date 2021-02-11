module Fission.User.Username.Types
  ( Username
  , mkUsername
  ) where

import           Database.Persist.Class
import           Database.Persist.Sql

import           Data.Swagger
import           Servant.API

import qualified RIO.ByteString.Lazy         as Lazy
import qualified RIO.Text                    as Text

import qualified Network.IPFS.Internal.UTF8  as UTF8

import           Fission.Prelude

import           Fission.URL.Validation
import           Fission.User.Username.Error

newtype Username = Username { username :: Text }
  deriving newtype ( Show
                   , Eq
                   , Display
                   , IsString
                   )

mkUsername :: Text -> Either Invalid Username
mkUsername txt =
  if isValid normalized
    then Right $ Username normalized
    else Left Invalid

  where
    normalized = Text.toLower txt

instance Arbitrary Username where
  arbitrary = do
    txt <- arbitrary
    case mkUsername $ Text.filter isURLChar txt of
      Left _      -> arbitrary
      Right uName -> return uName

instance ToParamSchema Username where
  toParamSchema _ = mempty |> type_ ?~ SwaggerString

instance ToHttpApiData Username where
  toUrlPiece (Username raw) = raw

instance PersistField Username where
  toPersistValue (Username un) = PersistText un
  fromPersistValue = \case
    input@(PersistText txt) ->
      -- NOTE this may break against old DB records, but we really should keep this strict
      -- I'd rather fail fast so know about it so we can fix it ~expede
      case mkUsername txt of
        Left _      -> Left $ errMsg input
        Right uName -> Right uName

    other ->
      Left $ errMsg other

    where
      errMsg input = "Invalid Persistent Username: " <> Text.pack (show input)

instance PersistFieldSql Username where
  sqlType _pxy = SqlString

instance ToJSON Username where
  toJSON (Username username) = String username

instance FromJSON Username where
  parseJSON = withText "Username" \txt ->
    case mkUsername txt of
      Left _      -> fail . Text.unpack $ "Invalid username: " <> txt
      Right uName -> return uName

instance FromHttpApiData Username where
  parseUrlPiece txt =
    case mkUsername txt of
      Left _err   -> Left $ "Invalid usrename: " <> txt
      Right uName -> Right uName

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
    case decodeUtf8' $ Lazy.toStrict bs of
      Left unicodeErr ->
        Left $ mconcat
          [ "Username "
          , show bs
          , " contains invalid non-unicode character(s): "
          , show unicodeErr
          ]

      Right txt ->
        case mkUsername txt  of
          Left  _err  -> Left . show $ "Invalid username: " <> bs
          Right uName -> Right uName
