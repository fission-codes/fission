{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.UCAN.Internal.Orphanage.Ed25519.Signature () where

import           Data.Aeson
import           RIO
import qualified RIO.Text                     as Text

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519        as Ed25519

import qualified Data.ByteString.Base64.URL   as BS.B64.URL

import qualified Web.UCAN.Internal.Base64     as Base64
import qualified Web.UCAN.Internal.Base64.URL as Base64.URL
import qualified Web.UCAN.Internal.Crypto     as Crypto

instance Display Ed25519.Signature where
  textDisplay sig =
    sig
      & Base64.toByteString
      & BS.B64.URL.encodeUnpadded
      & decodeUtf8Lenient
      -- Initial human readable text
      & stripOptionalPrefix "Signature \""
      & stripOptionalPrefix "\""
      -- End quotes
      & stripOptionalSuffix "\""
      & stripOptionalSuffix "\""
    where
      stripOptionalPrefix pfx txt = maybe txt id $ Text.stripPrefix pfx txt
      stripOptionalSuffix sfx txt = maybe txt id $ Text.stripSuffix sfx txt

instance ToJSON Ed25519.Signature where
  toJSON = String . textDisplay

instance FromJSON Ed25519.Signature where
  parseJSON = withText "Ed25519.Signature" \txt ->
    case Crypto.base64ToEd25519Signature . encodeUtf8 $ Base64.URL.decode txt of
      CryptoFailed err ->
        fail $ "Unable to parse as Ed25519 signature (" <> show err <> ") " <> show txt

      CryptoPassed sig ->
        return sig
