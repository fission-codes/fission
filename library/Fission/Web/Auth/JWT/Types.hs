module Fission.Web.Auth.JWT.Types
  ( JWT (..)

  -- * reexports
 
  , module Fission.Web.Auth.JWT.Claims.Types
  , module Fission.Web.Auth.JWT.Header.Types
  ) where

import           Crypto.PubKey.Ed25519 (toPublic)

import Data.Aeson as JSON

import qualified Fission.Internal.Base64 as B64

import Fission.Internal.RSA2048.Pair.Types

import qualified System.IO.Unsafe as Unsafe
import qualified Data.ByteString.Base64.URL as B64URL
 
import           Fission.Web.Auth.JWT.Signature.Types       as Signature

import qualified Data.ByteString.Lazy.Char8 as Char8

import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.List            as List
import RIO.Char (ord)

import           Fission.Prelude
import qualified Fission.Internal.UTF8 as UTF8
 
import qualified Fission.Key as Key
 
import qualified Fission.Key.Asymmetric.Algorithm.Types as Alg
import qualified Data.ByteString.Base64.URL.Lazy as B64
import Fission.User.DID.Types

import           Fission.Web.Auth.JWT.Claims.Types
import           Fission.Web.Auth.JWT.Header.Types (Header (..))
 
import           Fission.Web.Auth.JWT.Signature         as Signature
import qualified Fission.Web.Auth.JWT.Signature.RS256   as RS256
import qualified Fission.Web.Auth.JWT.Signature.Ed25519 as Ed25519

import Fission.Internal.Orphanage.Ed25519.SecretKey ()
 
import qualified RIO.Text.Partial as PText

-- | An RFC 7519 extended with support for Ed25519 keys,
--    and some specifics (claims, etc) for Fission's use case
data JWT = JWT
  { header :: !Header
  , claims :: !Claims
  , sig    :: !Signature
  } deriving (Show, Eq)

instance Arbitrary JWT where
  arbitrary = do
    header  <- arbitrary
    claims' <- arbitrary

    case alg header of
      Alg.Ed25519 -> do
        sk <- arbitrary

        let
          pkBS :: ByteString = B64.toB64ByteString $ toPublic sk

          did = DID
            { publicKey = Key.Public $ decodeUtf8Lenient pkBS
            , algorithm = Alg.Ed25519
            , method    = Key
            }

          claims = claims' { iss = did }
          sig    = Ed25519.sign header claims sk
         
        return JWT {..}

      Alg.RSA2048 ->
        genRSA header claims'
  
genRSA :: Header -> Claims -> Gen JWT
genRSA header claims' = do
  Pair _pk sk <- arbitrary

  let
    pk = Key.Public "FAKE_publickey"

    did = DID
      { publicKey = pk
      , algorithm = Alg.RSA2048
      , method    = Key
      }

    claims = claims' { iss = did }

  return (Unsafe.unsafePerformIO (RS256.sign header claims sk)) >>= \case
    Right sig -> return JWT {..}
    Left  _   -> genRSA header claims

instance ToJSON JWT where
  toJSON JWT {..} = String . decodeUtf8Lenient $
    encodeB64 header <> "." <> encodeB64 claims <> "." <> signed
    where
      signed :: ByteString
      signed =
        case sig of
          Ed25519 edSig                 -> encodeSig edSig
          RS256 (RS256.Signature rsSig) -> encodeSig rsSig

      encodeSig raw = -- FIXME extract
        raw
          |> B64.toB64ByteString
          |> decodeUtf8Lenient
          |> toURLEncoding
          |> encodeUtf8
          |> stripPadding

      encodeB64 :: ToJSON a => a -> ByteString -- FIXME extract
      encodeB64 jsonable =
        jsonable
          |> JSON.encode
          |> Lazy.toStrict
          |> stripQuotes
          |> B64URL.encode
          |> stripPadding

      toURLEncoding :: Text -> Text
      toURLEncoding = PText.replace "+" "-" . PText.replace "/" "_"

      stripQuotes :: ByteString -> ByteString
      stripQuotes = UTF8.stripOptionalPrefixBS "\"" . UTF8.stripOptionalSuffixBS "\""

      stripPadding :: ByteString -> ByteString -- FIXME extract!
      stripPadding  =
          UTF8.stripOptionalSuffixBS "=" -- per RFC7515
        . UTF8.stripOptionalSuffixBS "=" -- incase they trail
        . UTF8.stripOptionalSuffixBS "=" -- incase they trail

instance FromJSON JWT where
  parseJSON = withText "JWT.Token" \txt ->
    case Char8.split '.' . Lazy.fromStrict $ encodeUtf8 $ PText.replace "=" "" txt of
      [rawHeader, rawClaims, rawSig] -> do
        let
          result = do
            header <- addPadding rawHeader
            claims <- addPadding rawClaims
            sig    <- Signature.parse (alg header) $  "\"" <> rawSig <> "\""
            return JWT {..}

        case result of
          Left  err   -> fail err
          Right token -> return token

      _ ->
        fail $ show txt <> " is not a valid JWT.Token"

-- FIXME extract... but alos double check it's actually required. I think t's probbaly not
addPadding :: FromJSON x => Lazy.ByteString -> Either String x
addPadding bs = eitherDecode $ B64.decodeLenient (Lazy.pack padded)
  where
    n :: Int
    n = rem (fromIntegral $ Lazy.length bs) 4

    padded :: [Word8]
    padded = Lazy.unpack bs <> take n (List.repeat $ fromIntegral $ ord '=')
