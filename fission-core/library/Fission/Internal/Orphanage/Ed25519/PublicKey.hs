{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.Ed25519.PublicKey () where

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519  as Ed25519

import qualified Data.ByteString.Base64 as BS64
import qualified RIO.Text               as Text

import           Servant.API

import           Fission.Prelude
import           Fission.Internal.Orphanage.Ed25519.SecretKey ()
import qualified Fission.Internal.Base64 as B64
 
instance Display Ed25519.PublicKey where
  textDisplay = decodeUtf8Lenient . B64.toB64ByteString

instance ToHttpApiData Ed25519.PublicKey where
  toUrlPiece = textDisplay

instance FromHttpApiData Ed25519.PublicKey where
  parseUrlPiece txt =
    case Ed25519.publicKey . BS64.decodeLenient $ encodeUtf8 txt of
      CryptoPassed pk -> Right pk
      err -> Left $ "Unable to decode Ed25519 PK because: " <> Text.pack (show err) <> " / " <> txt

instance FromJSON Ed25519.PublicKey where
  parseJSON = withText "Ed25519.PublicKey" \txt ->
    case parseUrlPiece txt of
      Right pk -> return pk
      Left msg -> fail $ Text.unpack msg

instance ToJSON Ed25519.PublicKey where
  toJSON = String . textDisplay

instance Arbitrary Ed25519.PublicKey where
  arbitrary = Ed25519.toPublic <$> arbitrary
