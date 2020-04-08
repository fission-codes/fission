module Fission.Web.Auth.JWT.Types
  ( JWT (..)

  -- * reexports
 
  , module Fission.Web.Auth.JWT.Claims.Types
  , module Fission.Web.Auth.JWT.Header.Types
  ) where

import qualified System.IO.Unsafe as Unsafe
 
import           Crypto.PubKey.Ed25519 (toPublic)

import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Lazy.Char8 as Char8

import qualified RIO.ByteString.Lazy as Lazy

import           Fission.Prelude
import qualified Fission.Internal.UTF8 as UTF8
 
import qualified Fission.Key                            as Key
import qualified Fission.Key.Asymmetric.Algorithm.Types as Alg
import           Fission.User.DID.Types

import           Fission.Web.Auth.JWT.Claims.Types
import           Fission.Web.Auth.JWT.Header.Types (Header (..))

import           Fission.Web.Auth.JWT.Signature         as Signature
import qualified Fission.Web.Auth.JWT.Signature.RS256   as RS256
import qualified Fission.Web.Auth.JWT.Signature.Ed25519 as Ed25519

import qualified Fission.Internal.Base64     as B64
import qualified Fission.Internal.Base64.URL as BS64.URL

import           Fission.Internal.Orphanage.Ed25519.SecretKey ()
import           Fission.Internal.RSA2048.Pair.Types

-- | An RFC 7519 extended with support for Ed25519 keys,
--     and some specifics (claims, etc) for Fission's use case
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

  case Unsafe.unsafePerformIO $ RS256.sign header claims sk of
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

      encodeSig raw =
        raw
          |> B64.toB64ByteString
          |> B64URL.encode
          |> UTF8.stripPadding

      encodeB64 jsonable =
        jsonable
          |> encode
          |> Lazy.toStrict
          |> UTF8.stripQuotes
          |> B64URL.encode
          |> UTF8.stripPadding

instance FromJSON JWT where
  parseJSON = withText "JWT.Token" \txt ->
    case Char8.split '.' . Lazy.fromStrict $ UTF8.stripPadding $ encodeUtf8 $ txt of
      [rawHeader, rawClaims, rawSig] -> do
        let
          result = do
            header <- BS64.URL.addPadding rawHeader
            claims <- BS64.URL.addPadding rawClaims
            sig    <- Signature.parse (alg header) $  "\"" <> rawSig <> "\""
            return JWT {..}

        case result of
          Left  err   -> fail err
          Right token -> return token

      _ ->
        fail $ show txt <> " is not a valid JWT.Token"
