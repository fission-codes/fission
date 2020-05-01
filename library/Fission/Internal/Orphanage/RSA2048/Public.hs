{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.RSA2048.Public () where

import qualified Crypto.PubKey.RSA as RSA
import qualified System.IO.Unsafe  as Unsafe

import           Servant.API

import           Fission.Prelude
 

-- import           Crypto.Error
-- import qualified Crypto.PubKey.Ed25519 as Crypto.Ed25519
-- import qualified Crypto.PubKey.RSA     as Crypto.RSA
import qualified Crypto.Store.X509     as X509

import qualified Data.ASN1.BinaryEncoding as ASN1
import qualified Data.ASN1.Encoding       as ASN1
import qualified Data.ASN1.Types          as ASN1

import qualified Data.PEM as PEM

-- import           Data.Swagger
-- import           Database.Persist.Postgresql

import qualified Data.ByteString.Base64 as BS64
import qualified Data.X509              as X509

import qualified RIO.Text as Text
-- import           Servant.API

-- import           Fission.Prelude hiding (length)

-- import           Fission.Internal.Base64.Scrubbed    as B64.Scrubbed
-- import qualified Fission.Internal.Base64             as B64
-- import           Fission.Internal.RSA2048.Pair.Types as Pair
 
instance Display RSA.PublicKey where
  textDisplay pk =
    X509.PubKeyRSA pk
      |> X509.pubKeyToPEM
      |> PEM.pemWriteBS
      |> decodeUtf8Lenient
      |> Text.strip
      |> Text.dropPrefix pemHeader
      |> Text.dropSuffix pemFooter
      |> Text.filter (/= '\n')

instance ToHttpApiData RSA.PublicKey where
  toUrlPiece = textDisplay
 
instance FromHttpApiData RSA.PublicKey where
  parseUrlPiece txt =
    case ASN1.fromASN1 <$> ASN1.decodeASN1' ASN1.DER (BS64.decodeLenient $ encodeUtf8 txt) of
      Right (Right (X509.PubKeyRSA pk, _)) -> Right pk
      err -> Left $ "Cannot parse RSA key because: " <> Text.pack (show err) <> " / " <> txt

instance FromJSON RSA.PublicKey where
  parseJSON = withText "RSA2048.Public" \txt ->
    case parseUrlPiece txt of
      Right pk -> return pk
      Left msg -> fail $ Text.unpack msg

instance ToJSON RSA.PublicKey where
  toJSON = String . textDisplay

-- NOTE TO SELF
-- Quickcheck taking forever? Use RSA2048/Pair/Types.hs!
-- Anything over 256 bits takes _forever_
instance Arbitrary RSA.PublicKey where
  arbitrary = do
    exp <- elements [3, 5, 17, 257, 65537]
    return . fst . Unsafe.unsafePerformIO $ RSA.generate 2048 exp

pemHeader :: Text
pemHeader = "-----BEGIN PUBLIC KEY-----"

pemFooter :: Text
pemFooter = "-----END PUBLIC KEY-----"
