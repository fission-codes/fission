module Fission.Internal.Crypto
  ( pack
  , unpack
  , base64ToSignature
  , base64ToEdPubKey
  , toBase64 
  ) where

import Fission.Prelude

import qualified RIO.ByteString         as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteArray         as BA

import qualified Crypto.PubKey.Ed25519 as Ed
import           Crypto.Error

pack :: ByteString -> BA.ScrubbedBytes
pack = BA.pack . BS.unpack

unpack :: BA.ByteArrayAccess a => a -> ByteString 
unpack = BS.pack . BA.unpack

base64ToSignature :: ByteString -> CryptoFailable Ed.Signature
base64ToSignature = edSignature . base64ToBytes 

base64ToEdPubKey :: ByteString -> CryptoFailable Ed.PublicKey
base64ToEdPubKey = Ed.publicKey . base64ToBytes 

base64ToBytes :: ByteString -> BA.ScrubbedBytes
base64ToBytes = pack . Base64.decodeLenient

toBase64 :: BA.ByteArrayAccess a => a -> ByteString
toBase64 = Base64.encode . unpack

edSignature :: BA.ByteArrayAccess ba => ba -> CryptoFailable Ed.Signature
edSignature = Ed.signature
