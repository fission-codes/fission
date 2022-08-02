module Crypto.Key.Asymmetric.Public.Types (Public (..)) where

import qualified Crypto.PubKey.Ed25519                            as Crypto.Ed25519
import qualified Crypto.PubKey.RSA                                as Crypto.RSA
import qualified Crypto.Secp256k1

import           Data.Swagger
import           Database.Persist.Sql

import           Control.Lens                                     ((?~))
import           Data.Aeson                                       as Json
import qualified Data.Aeson.Types                                 as Json
import           RIO
import qualified RIO.ByteString.Lazy                              as Lazy
import qualified RIO.Text                                         as Text
import           Servant.API
import           Test.QuickCheck

import           Web.UCAN.Internal.RSA2048.Pair.Types             as Pair

import           Web.UCAN.Internal.Orphanage.Ed25519.PublicKey    ()
import           Web.UCAN.Internal.Orphanage.RSA2048.Public       ()
import           Web.UCAN.Internal.Orphanage.Secp256k1.PublicKey  ()

{-| Public key type.
-}
data Public
  = Ed25519PublicKey   Crypto.Ed25519.PublicKey
  | RSAPublicKey       Crypto.RSA.PublicKey
  | Secp256k1PublicKey Crypto.Secp256k1.PubKeyXY
  deriving (Eq)

instance Show Public where
  show = \case
    Ed25519PublicKey   ed -> Text.unpack $ textDisplay ed
    RSAPublicKey       pk -> show pk
    Secp256k1PublicKey pk -> Text.unpack $ textDisplay pk

instance Display Public where
  textDisplay (Ed25519PublicKey   pk) = textDisplay pk
  textDisplay (RSAPublicKey       pk) = textDisplay pk
  textDisplay (Secp256k1PublicKey pk) = textDisplay pk

instance Arbitrary Public where
  arbitrary = oneof
    [ Ed25519PublicKey <$> arbitrary
    , RSAPublicKey . Pair.pk <$> arbitrary
    , Secp256k1PublicKey <$> arbitrary
    ]

instance ToHttpApiData Public where
  toUrlPiece = textDisplay

instance FromHttpApiData Public where
  parseUrlPiece text =
    case (Json.decode . Lazy.fromStrict . Text.encodeUtf8 $ text) :: Maybe Public of
      Just pub -> Right pub
      Nothing -> Left "Failed to decode the JSON formatted public key"

instance IsString (Either Text Public) where
  fromString = parseUrlPiece . Text.pack

instance FromJSON Public where
  parseJSON (Object j) =
    ((,) <$> j .: "type" <*> j .: "key") >>= \(t, k) ->
    case fromKeyType t k of
      Right pub -> return pub
      Left err -> Json.parseFail (Text.unpack err)

  parseJSON value =
    withText
      "PublicKey"
      (\text ->
        case
          if "MII" `Text.isPrefixOf` text
          then RSAPublicKey     <$> parseUrlPiece text
          else Ed25519PublicKey <$> parseUrlPiece text
        of
          Right pk -> return pk
          Left msg -> fail $ Text.unpack msg
      )
      value

instance ToJSON Public where
  toJSON key = object
    [ "type" .= toKeyType key
    , "key"  .= textDisplay key
    ]

instance PersistField Public where
  toPersistValue =
    PersistText . decodeUtf8Lenient . Lazy.toStrict . Json.encode . toJSON

  fromPersistValue (PersistText text) =
    parseUrlPiece text

  fromPersistValue other =
    Left $ "Invalid Persistent PK: " <> Text.pack (show other)

instance PersistFieldSql Public where
  sqlType _pxy = SqlString

instance ToSchema Public where
  declareNamedSchema _ =
    mempty
      & type_ ?~ SwaggerString
      & description ?~ "A public key"
      & NamedSchema (Just "PublicKey")
      & pure



-- CONSTRUCTORS


fromKeyType :: Text -> Text -> Either Text Public
fromKeyType "Ed25519"   = fmap Ed25519PublicKey . parseUrlPiece
fromKeyType "RSA"       = fmap RSAPublicKey . parseUrlPiece
fromKeyType "Secp256k1" = fmap Secp256k1PublicKey . parseUrlPiece
fromKeyType _           = \_ -> Left "Invalid Public.Key type"

toKeyType :: Public -> Text
toKeyType (Ed25519PublicKey _)   = "Ed25519"
toKeyType (RSAPublicKey _)       = "RSA"
toKeyType (Secp256k1PublicKey _) = "Secp256k1"
