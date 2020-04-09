module Fission.Internal.Crypto
  ( base64ToEd25519Signature
  , base64ToEd25519PK
  , decodeToRSA2048PK
  ) where

import qualified Data.ASN1.BinaryEncoding as ASN1
import qualified Data.ASN1.Encoding       as ASN1
import qualified Data.ASN1.Types          as ASN1
 
import qualified Data.X509                 as X509
-- import qualified Data.Base58String.Bitcoin as BS58.BTC

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519   as Crypto.Ed25519
import qualified Crypto.PubKey.RSA.Types as Crypto.RSA

import           Fission.Prelude
import           Fission.Internal.Base64.Scrubbed as B64.Scrubbed





-- import Data.ASN1.Prim
-- import Crypto.Number.Serialize

base64ToEd25519Signature :: ByteString -> CryptoFailable Crypto.Ed25519.Signature
base64ToEd25519Signature = Crypto.Ed25519.signature . B64.Scrubbed.scrubB64

base64ToEd25519PK :: ByteString -> CryptoFailable Crypto.Ed25519.PublicKey
base64ToEd25519PK = Crypto.Ed25519.publicKey . B64.Scrubbed.scrubB64

-- Operates on non-base58
decodeToRSA2048PK :: ByteString -> Either String Crypto.RSA.PublicKey
decodeToRSA2048PK raw =
  case ASN1.fromASN1 <$> ASN1.decodeASN1' ASN1.DER raw of
    Right (Right (X509.PubKeyRSA pk, _)) -> Right pk
    err -> Left $ "Unable to decode as RSA 2048 key: " <> show err -- raw
