module Fission.Key.Asymmetric.Public.Types
  ( Public (..)
  -- , Algorithm (..)
  ) where

-- import           Data.ByteArray

import qualified Data.ASN1.BinaryEncoding as ASN1
import qualified Data.ASN1.Encoding       as ASN1
import qualified Data.ASN1.Types          as ASN1

import qualified Data.X509 as X509

import qualified Fission.Internal.Base64     as B64
import qualified Fission.Internal.Base64.Scrubbed as B64.Scrubbed

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519    as Crypto.Ed25519
import qualified Crypto.PubKey.RSA        as Crypto.RSA

import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import Servant.API

import           Data.Swagger
import           Database.Persist.Postgresql

import           Fission.Prelude hiding (length)

import Fission.Internal.Orphanage.RSA2048.Public ()
import Fission.Internal.Orphanage.Ed25519.PublicKey ()

import qualified Data.Binary as Binary

import qualified Codec.Crypto.RSA.Pure as RSA

data Public
  = Ed25519PublicKey Crypto.Ed25519.PublicKey -- Text REMONDER ensure that raw is-- BS64 encoded
  | RSAPublicKey     Crypto.RSA.PublicKey     -- Text -- BS64 encoded
  deriving Eq

instance Show Public where
  show = Text.unpack . textDisplay

instance Display Public where
  textDisplay (RSAPublicKey Crypto.RSA.PublicKey {..}) =
    decodeUtf8Lenient . Lazy.toStrict . Binary.encode $ RSA.PublicKey {..}

  textDisplay (Ed25519PublicKey pk) =
    decodeUtf8Lenient $ B64.toB64ByteString pk

instance Arbitrary Public where
  arbitrary = oneof
    [ Ed25519PublicKey <$> arbitrary
    , RSAPublicKey     <$> arbitrary
    ]

instance ToHttpApiData Public where
  toUrlPiece = textDisplay

instance FromHttpApiData Public where
  parseUrlPiece txt =
    if Text.length txt < 88 -- NOTE Ed25519 is 44. 88 for some buffer space.
      then
        case Crypto.Ed25519.publicKey . B64.Scrubbed.scrubB64 $ encodeUtf8 txt of
          CryptoPassed pk -> Right $ Ed25519PublicKey pk
          _ -> Left $ "Unable to decode Ed25519 PK: " <> txt

      else
        case ASN1.fromASN1 <$> ASN1.decodeASN1' ASN1.DER (encodeUtf8 txt) of
          Right (Right (X509.PubKeyRSA pk, _)) -> Right $ RSAPublicKey pk
          _ -> Left $ "Unable to decode as RSA 2048 key: " <> txt

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
  toPersistValue = PersistText . textDisplay

  fromPersistValue (PersistText txt) = parseUrlPiece txt
  fromPersistValue other = Left $ "Invalid Persistent PK: " <> Text.pack (show other)

instance PersistFieldSql Public where
  sqlType _pxy = SqlString

instance ToSchema Public where
  declareNamedSchema _ =
    mempty
      |> type_ ?~ SwaggerString
      |> description ?~ "A public key"
      |> NamedSchema (Just "PublicKey")
      |> pure

