{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.Ed25519.Signature () where

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519 as Ed25519

import qualified Data.ByteString.Base64.URL as BS.B64.URL
 
import           Fission.Prelude

import qualified Fission.Internal.Crypto     as Crypto
import qualified Fission.Internal.UTF8       as UTF8
import qualified Fission.Internal.Base64.URL as Base64.URL
import qualified Fission.Internal.Base64     as Base64

instance Display Ed25519.Signature where
  textDisplay sig =
    sig
      |> Base64.toByteString
      |> BS.B64.URL.encodeUnpadded
      |> decodeUtf8Lenient
      -- Initial human readable text
      |> UTF8.stripOptionalPrefix "Signature \""
      |> UTF8.stripOptionalPrefix "\""
      -- End quotes
      |> UTF8.stripOptionalSuffix "\""
      |> UTF8.stripOptionalSuffix "\""

instance ToJSON Ed25519.Signature where
  toJSON = String . textDisplay

instance FromJSON Ed25519.Signature where
  parseJSON = withText "Ed25519.Signature" \txt ->
    case Crypto.base64ToEd25519Signature . encodeUtf8 $ Base64.URL.decode txt of
      CryptoFailed err ->
        fail $ "Unable to parse as Ed25519 signature (" <> show err <> ") " <> show txt

      CryptoPassed sig ->
        return sig
