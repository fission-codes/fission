module Fission.Internal.Crypto
  ( base64ToEd25519Signature
  , base64ToEd25519PK
  ) where

import qualified Data.ASN1.BinaryEncoding as ASN1
import qualified Data.ASN1.Encoding       as ASN1
import qualified Data.ASN1.Types          as ASN1
 
import qualified Data.X509 as X509

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519   as Crypto.Ed25519
import qualified Crypto.PubKey.RSA.Types as Crypto.RSA

import           Fission.Prelude
import           Fission.Internal.Base64.Scrubbed as B64.Scrubbed

base64ToEd25519Signature :: ByteString -> CryptoFailable Crypto.Ed25519.Signature
base64ToEd25519Signature = Crypto.Ed25519.signature . B64.Scrubbed.scrubB64

base64ToEd25519PK :: ByteString -> CryptoFailable Crypto.Ed25519.PublicKey
base64ToEd25519PK = Crypto.Ed25519.publicKey . B64.Scrubbed.scrubB64
