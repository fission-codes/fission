module Fission.CLI.Key.Ed25519 (parseSecretKey) where

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519 as Ed25519

import qualified Data.ByteArray        as ByteArray

import           Fission.Prelude

import           Fission.Key.Error     as Key

parseSecretKey ::
  ( MonadRaise m
  , Raises     m Key.Error
  )
  => ByteArray.ScrubbedBytes
  -> m Ed25519.SecretKey
parseSecretKey = ensure . parseKey Ed25519.secretKey

-- TODO remove as part of cli-linking

parseKey ::
     (ByteArray.ScrubbedBytes -> CryptoFailable a)
  -> ByteArray.ScrubbedBytes
  -> Either Key.Error a
parseKey f bytes =
  case f bytes of
    CryptoPassed sk  -> Right sk
    CryptoFailed err -> Left $ Key.ParseError err
