{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.Ed25519.Signature () where

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519 as Ed25519

import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Base64 as Base64

import qualified RIO.ByteString as BS
import qualified RIO.Text as Text

import RIO.Char
import qualified Data.ByteArray         as BA

import           Fission.Prelude
 
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base64.URL as B64URL

import qualified RIO.Text.Partial as PText

import qualified Fission.Internal.Crypto as Crypto
import qualified Fission.Internal.UTF8   as UTF8
 
instance ToJSON Ed25519.Signature where
  toJSON sig =
    sig
      |> Crypto.toBase64
      |> decodeUtf8Lenient
      |> UTF8.stripOptionalPrefix "Signature \""
      |> UTF8.stripOptionalPrefix "\""
      |> UTF8.stripOptionalSuffix "\""
      |> UTF8.stripOptionalSuffix "\""
      |> String

instance FromJSON Ed25519.Signature where
  parseJSON = withText "Ed25519.Signature" \txt ->
    txt
      |> fromURLEncoding
      |> encodeUtf8
      |> Crypto.base64ToSignature
      |> \case
          CryptoFailed err ->
            fail $ "Unable to parse as Ed25519 signature (" <> show err <> ") " <> show txt

          CryptoPassed sig ->
            return sig

fromURLEncoding = PText.replace "-" "+" . PText.replace "_" "/"
