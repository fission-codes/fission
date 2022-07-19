{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.UCAN.Internal.Orphanage.Secp256k1.PublicKey () where

import qualified Crypto.Secp256k1                                 as Secp256k1

import           Data.ByteArray                                   as ByteArray
import qualified Data.ByteString.Base64                           as BS64
import qualified RIO.Text                                         as Text

import           Servant.API

import qualified Web.UCAN.Internal.Base64                         as B64
import           Web.UCAN.Internal.Orphanage.Secp256k1.SecretKey ()

import           Data.Aeson
import           RIO
import           Test.QuickCheck

instance Arbitrary Secp256k1.PubKeyXY where
  arbitrary = Secp256k1.derivePubKey <$> arbitrary

instance ByteArrayAccess Secp256k1.PubKeyXY where
  -- Bool argument to `exportPubKeyXY` is wether or not to enable compression
  -- 33 bytes compressed, 65 bytes uncompressed
  length        = ByteArray.length . Secp256k1.exportPubKeyXY True
  withByteArray = withByteArray    . Secp256k1.exportPubKeyXY True

instance Display Secp256k1.PubKeyXY where
  textDisplay = decodeUtf8Lenient . B64.toB64ByteString

instance ToHttpApiData Secp256k1.PubKeyXY where
  toUrlPiece = textDisplay

instance FromHttpApiData Secp256k1.PubKeyXY where
  parseUrlPiece txt =
    case Secp256k1.importPubKeyXY . BS64.decodeLenient $ encodeUtf8 txt of
      Just pk -> Right pk
      Nothing -> Left $ "Unable to decode Secp256k1 PK / " <> txt

instance FromJSON Secp256k1.PubKeyXY where
  parseJSON = withText "Secp256k1.PubKeyXY" \txt ->
    case parseUrlPiece txt of
      Right pk -> return pk
      Left msg -> fail $ Text.unpack msg

instance ToJSON Secp256k1.PubKeyXY where
  toJSON = String . textDisplay
