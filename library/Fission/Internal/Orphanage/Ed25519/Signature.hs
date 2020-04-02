{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.Ed25519.Signature () where

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519 as Ed25519

import           Fission.Prelude

import qualified Fission.Internal.Crypto as Crypto

instance FromJSON Ed25519.Signature where
  parseJSON = withText "Ed25519.Signature" \txt ->
    case Crypto.base64ToSignature (encodeUtf8 txt) of
      CryptoFailed err ->
        fail $ "Unable to parse as Ed25519 signature (" <> show err <> ") " <> show txt
 
      CryptoPassed sig ->
        return sig

instance ToJSON Ed25519.Signature where
  toJSON = String . decodeUtf8Lenient . Crypto.toBase64
