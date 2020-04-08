{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.Ed25519.Signature () where

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519 as Ed25519
 
import           Fission.Prelude

import qualified Fission.Internal.Crypto     as Crypto
import qualified Fission.Internal.UTF8       as UTF8
import qualified Fission.Internal.Base64.URL as Base64.URL
import qualified Fission.Internal.Base64     as Base64

instance ToJSON Ed25519.Signature where
  toJSON sig =
    sig
      |> Base64.toB64ByteString
      |> decodeUtf8Lenient
      |> UTF8.stripOptionalPrefix "Signature \""
      |> UTF8.stripOptionalPrefix "\""
      |> UTF8.stripOptionalSuffix "\""
      |> UTF8.stripOptionalSuffix "\""
      |> String

instance FromJSON Ed25519.Signature where
  parseJSON = withText "Ed25519.Signature" \txt ->
    txt
      |> Base64.URL.decode
      |> encodeUtf8
      |> Crypto.base64ToEd25519Signature
      |> \case
          CryptoFailed err ->
            fail $ "Unable to parse as Ed25519 signature (" <> show err <> ") " <> show txt

          CryptoPassed sig ->
            return sig
