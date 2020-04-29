module Fission.Key.Asymmetric.Public.Types (Public (..)) where

import qualified Codec.Crypto.RSA.Pure as RSA

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519 as Crypto.Ed25519
import qualified Crypto.PubKey.RSA     as Crypto.RSA

import qualified Data.ASN1.BinaryEncoding as ASN1
import qualified Data.ASN1.Encoding       as ASN1
import qualified Data.ASN1.Types          as ASN1

import qualified Data.Binary as Binary
import           Data.Swagger
import           Database.Persist.Postgresql

import qualified Data.ByteString.Base64 as BS64
import qualified Data.X509              as X509

import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import           Servant.API

import           Fission.Prelude hiding (length)

import           Fission.Internal.Base64.Scrubbed as B64.Scrubbed
import qualified Fission.Internal.Base64 as B64

import           Fission.Internal.Orphanage.RSA2048.Public    ()
import           Fission.Internal.Orphanage.Ed25519.PublicKey ()


import  qualified Data.ASN1.Types  as ASN1
import qualified Crypto.Store.X509 as X509
import Data.PEM as PEM
import Fission.Internal.RSA2048.Pair.Types as RSA2048

data Public
  = Ed25519PublicKey Crypto.Ed25519.PublicKey
  | RSAPublicKey     Crypto.RSA.PublicKey
  deriving Eq

instance Show Public where
  show = Text.unpack . textDisplay

instance Display Public where
  textDisplay (Ed25519PublicKey pk) =
    decodeUtf8Lenient $ B64.toB64ByteString pk

  textDisplay (RSAPublicKey pk) =
    X509.PubKeyRSA pk
      |> X509.pubKeyToPEM
      |> PEM.pemWriteBS
      |> decodeUtf8Lenient
      |> Text.strip
      |> Text.dropPrefix "-----BEGIN PUBLIC KEY-----"
      |> Text.dropSuffix "-----END PUBLIC KEY-----"
      |> Text.filter (/= '\n')

instance Arbitrary Public where
  arbitrary = oneof
    [ Ed25519PublicKey <$> arbitrary
    , pregenRSA
    ]
    where
      pregenRSA = do
        Pair pk _ <- arbitrary
        return $ RSAPublicKey pk

instance ToHttpApiData Public where
  toUrlPiece = textDisplay

instance FromHttpApiData Public where
  parseUrlPiece txt =
    if "MII" `Text.isPrefixOf` txt
      then
        case ASN1.fromASN1 <$> ASN1.decodeASN1' ASN1.DER (BS64.decodeLenient $ encodeUtf8 txt) of
          Right (Right (X509.PubKeyRSA pk, _)) -> Right $ RSAPublicKey pk
          err -> Left $ "Cannot parse RSA key because: " <> Text.pack (show err) <> " / " <> txt

      else
        case Crypto.Ed25519.publicKey . B64.Scrubbed.scrubB64 $ encodeUtf8 txt of
          CryptoPassed pk -> Right $ Ed25519PublicKey pk
          err -> Left $ "Unable to decode Ed25519 PK because: " <> Text.pack (show err) <> txt

instance IsString (Either Text Public) where
  fromString = parseUrlPiece . Text.pack

instance FromJSON Public where
  parseJSON = withText "PublicKey" \txt ->
    case parseUrlPiece txt of
      Right pk -> return pk
      Left msg -> fail $ Text.unpack msg

instance ToJSON Public where
  toJSON = String . textDisplay

instance PersistField Public where
  toPersistValue =
    PersistText . textDisplay

  fromPersistValue (PersistText txt) =
    parseUrlPiece txt
   
  fromPersistValue other =
    Left $ "Invalid Persistent PK: " <> Text.pack (show other)

instance PersistFieldSql Public where
  sqlType _pxy = SqlString

instance ToSchema Public where
  declareNamedSchema _ =
    mempty
      |> type_ ?~ SwaggerString
      |> description ?~ "A public key"
      |> NamedSchema (Just "PublicKey")
      |> pure

