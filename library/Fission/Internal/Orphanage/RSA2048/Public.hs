{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.RSA2048.Public () where

import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.Store.X509 as X509

import qualified Data.ASN1.BinaryEncoding as ASN1
import qualified Data.ASN1.Encoding       as ASN1
import qualified Data.ASN1.Types          as ASN1

import qualified Data.PEM as PEM

import qualified Data.ByteString.Base64 as BS64
import qualified Data.X509              as X509

import           Data.Swagger
import           Database.Persist.Postgresql

import qualified RIO.Text as Text
 
import           Servant.API

import           Fission.Prelude

instance Display RSA.PublicKey where
  textDisplay pk =
    X509.PubKeyRSA pk
      |> X509.pubKeyToPEM
      |> PEM.pemWriteBS
      |> decodeUtf8Lenient
      |> Text.strip
      |> Text.dropPrefix pemHeader
      |> Text.dropSuffix pemFooter
      |> Text.filter (/= '\n')

instance ToHttpApiData RSA.PublicKey where
  toUrlPiece = textDisplay
 
instance FromHttpApiData RSA.PublicKey where
  parseUrlPiece txt =
    case ASN1.fromASN1 <$> ASN1.decodeASN1' ASN1.DER (BS64.decodeLenient $ encodeUtf8 txt) of
      Right (Right (X509.PubKeyRSA pk, _)) -> Right pk
      err -> Left $ "Cannot parse RSA key because: " <> Text.pack (show err) <> " / " <> txt

instance FromJSON RSA.PublicKey where
  parseJSON = withText "RSA2048.Public" \txt ->
    case parseUrlPiece txt of
      Right pk -> return pk
      Left msg -> fail $ Text.unpack msg

instance ToJSON RSA.PublicKey where
  toJSON = String . textDisplay

instance PersistField RSA.PublicKey where
  toPersistValue =
    PersistText . textDisplay

  fromPersistValue (PersistText txt) =
    parseUrlPiece txt
   
  fromPersistValue other =
    Left $ "Invalid Persistent RSA PK: " <> Text.pack (show other)

instance ToParamSchema RSA.PublicKey where
  toParamSchema _ = mempty |> type_ ?~ SwaggerString

instance ToSchema RSA.PublicKey where
  declareNamedSchema _ =
    mempty
      |> type_ ?~ SwaggerString
      |> description ?~ "An RSA public key"
      |> NamedSchema (Just "RSA.PublicKey")
      |> pure

pemHeader :: Text
pemHeader = "-----BEGIN PUBLIC KEY-----"

pemFooter :: Text
pemFooter = "-----END PUBLIC KEY-----"
