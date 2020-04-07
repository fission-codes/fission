module Fission.Internal.Crypto
  ( pack
  , unpack
  , base64ToSignature
  , base64ToEdPubKey
  , base64ToBytes
  , toBase64
  ) where

import           Fission.Prelude

import qualified RIO.ByteString         as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteArray         as BA

import qualified Crypto.PubKey.Ed25519 as Crypto.Ed25519
import           Crypto.Error

pack :: ByteString -> BA.ScrubbedBytes
pack = BA.pack . BS.unpack

unpack :: BA.ByteArrayAccess a => a -> ByteString
unpack = BS.pack . BA.unpack

base64ToSignature :: ByteString -> CryptoFailable Crypto.Ed25519.Signature
base64ToSignature = Crypto.Ed25519.signature . base64ToBytes

base64ToEdPubKey :: ByteString -> CryptoFailable Crypto.Ed25519.PublicKey
base64ToEdPubKey = Crypto.Ed25519.publicKey . base64ToBytes

base64ToBytes :: ByteString -> BA.ScrubbedBytes
base64ToBytes = pack . Base64.decodeLenient

toBase64 :: BA.ByteArrayAccess a => a -> ByteString
toBase64 = Base64.encode . unpack
