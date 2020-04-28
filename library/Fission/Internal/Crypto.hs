module Fission.Internal.Crypto
  ( base64ToEd25519Signature
  , base64ToEd25519PK
  ) where

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519 as Crypto.Ed25519

import           Fission.Prelude
import           Fission.Internal.Base64.Scrubbed as B64.Scrubbed

base64ToEd25519Signature :: ByteString -> CryptoFailable Crypto.Ed25519.Signature
base64ToEd25519Signature = Crypto.Ed25519.signature . B64.Scrubbed.scrubB64

base64ToEd25519PK :: ByteString -> CryptoFailable Crypto.Ed25519.PublicKey
base64ToEd25519PK = Crypto.Ed25519.publicKey . B64.Scrubbed.scrubB64
