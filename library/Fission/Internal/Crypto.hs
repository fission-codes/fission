module Fission.Internal.Crypto
  ( base64ToEd25519Signature
  , base64ToEd25519PK
  , decodeToRSA2048PK
  , decodeB58ToRSA2048PK
  ) where

import qualified Crypto.PubKey.Ed25519 as Crypto.Ed25519
import           Crypto.Error
 
import           Fission.Prelude
import           Fission.Internal.Base64.Scrubbed as B64.Scrubbed

import qualified Codec.Crypto.RSA.Pure as RSA

import qualified Data.ASN1.Prim           as ASN1
import qualified Data.ASN1.Encoding       as ASN1
import qualified Data.ASN1.Types          as ASN1
import qualified Data.ASN1.BinaryEncoding as ASN1
import qualified Data.ASN1.Error          as ASN1
 
import qualified Data.X509 as X509

import qualified RIO.ByteString.Lazy as Lazy

import qualified Crypto.PubKey.RSA.Types as Crypto

import qualified Data.Base58String.Bitcoin as BS58.BTC

import qualified Crypto.Number.Serialize as Serialize

base64ToEd25519Signature :: ByteString -> CryptoFailable Crypto.Ed25519.Signature
base64ToEd25519Signature = Crypto.Ed25519.signature . B64.Scrubbed.scrubB64

base64ToEd25519PK :: ByteString -> CryptoFailable Crypto.Ed25519.PublicKey
base64ToEd25519PK = Crypto.Ed25519.publicKey . B64.Scrubbed.scrubB64

-- Operates on non-base58
decodeToRSA2048PK :: ByteString -> Either String Crypto.PublicKey
decodeToRSA2048PK raw =
  raw
    |> Serialize.os2ip -- ByteString -> Integer
    |> ASN1.putInteger
    |> ASN1.decodeASN1' ASN1.DER
    |> fmap ASN1.fromASN1
    |> \case
      Right (Right (X509.PubKeyRSA pk, _)) ->
        Right pk
 
      _ ->
        Left $ "Unable to decode as RSA 2048 key: " <> show raw

decodeB58ToRSA2048PK :: ByteString -> Either String Crypto.PublicKey
decodeB58ToRSA2048PK raw =
  raw
    |> decodeUtf8Lenient
    |> BS58.BTC.fromText
    |> BS58.BTC.toBytes
    |> decodeToRSA2048PK
