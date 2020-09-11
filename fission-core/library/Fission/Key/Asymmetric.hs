-- | Top-level assymmetric-key module

module Fission.Key.Asymmetric
  ( parseKey
  , signWith
  -- * Reexport
  , module Fission.Key.Asymmetric.Public.Types
  ) where

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519               as Ed25519

import qualified Data.ByteArray                      as ByteArray

import           Fission.Key.Error                   as Key
import           Fission.Prelude

import           Fission.Key.Asymmetric.Public.Types

signWith :: Ed25519.SecretKey -> ByteString -> Ed25519.Signature
signWith sk bs = Ed25519.sign sk (Ed25519.toPublic sk) bs

parseKey ::
     (ByteArray.ScrubbedBytes -> CryptoFailable a)
  -> ByteArray.ScrubbedBytes
  -> Either Key.Error a
parseKey f bytes =
  case f bytes of
    CryptoPassed sk  -> Right sk
    CryptoFailed err -> Left $ Key.ParseError err
