module Fission.Web.Auth.JWT.Types
  ( JWT (..)

  -- * reexports
 
  , module Fission.Web.Auth.JWT.Claims.Types
  , module Fission.Web.Auth.JWT.Header.Types
  ) where

import           Crypto.PubKey.Ed25519 (toPublic)
import qualified Crypto.PubKey.RSA     as RSA

import qualified Data.ByteString.Lazy.Char8 as Char8

import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text            as Text

import qualified System.IO.Unsafe as Unsafe

import           Fission.Prelude
import qualified Fission.Internal.UTF8 as UTF8
 
import qualified Fission.Key as Key
 
import qualified Fission.Key.Asymmetric.Algorithm.Types as Alg

import Fission.User.DID.Types

import           Fission.Web.Auth.JWT.Claims.Types
import           Fission.Web.Auth.JWT.Header.Types (Header (..))
 
import           Fission.Web.Auth.JWT.Signature         as Signature
import qualified Fission.Web.Auth.JWT.Signature.RS256   as RS256
import qualified Fission.Web.Auth.JWT.Signature.Ed25519 as Ed25519

import Fission.Internal.Orphanage.Ed25519.SecretKey ()

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
          pk =
            sk
              |> toPublic
              |> show
              |> Text.pack
              |> UTF8.stripOptionalPrefix "\""
              |> UTF8.stripOptionalSuffix "\""
              |> Key.Public

          did = DID
            { publicKey = pk
            , algorithm = Alg.Ed25519
            , method    = Key
            }

          claims = claims' { iss = did }
          sig    = Ed25519.sign header claims sk
         
        return JWT {..}

      Alg.RSA2048 -> do
        exp <- elements [3, 5, 17, 257, 65537]
 
        let
          (pk', sk) = Unsafe.unsafePerformIO $ RSA.generate 2048 exp

          pk =
            pk'
              |> show
              |> Text.pack
              |> UTF8.stripOptionalPrefix "\""
              |> UTF8.stripOptionalSuffix "\""
              |> Key.Public

          did = DID
            { publicKey = pk
            , algorithm = Alg.RSA2048
            , method    = Key
            }

          claims = claims' { iss = did }
       
        return (Unsafe.unsafePerformIO (RS256.sign header claims sk)) >>= \case
          Right sig -> return JWT {..}
          Left  _   -> arbitrary -- try again

instance ToJSON JWT where
  toJSON JWT {..} = String encoded
    where
      encoded :: Text
      encoded = decodeUtf8Lenient (toStrictBytes payload) <> signed

      signed :: Text
      signed =
        UTF8.stripOptionalPrefix "\"" . UTF8.stripOptionalSuffix "\"" $
          case sig of
            Ed25519 edSig -> Text.pack $ show edSig
            RS256 (RS256.Signature rsSig) -> decodeUtf8Lenient rsSig

      payload :: Lazy.ByteString
      payload = encode header <> "." <> encode claims <> "."

instance FromJSON JWT where
  parseJSON = withText "JWT.Token" \txt ->
    case Char8.split '.' . Lazy.fromStrict $ encodeUtf8 txt of
      [rawHeader, rawClaims, rawSig] -> do
        let
          result = do
            header <- eitherDecode rawHeader
            claims <- eitherDecode rawClaims
            sig    <- Signature.parse (alg header) rawSig
            return JWT {..}

        case result of
          Left  err   -> fail err
          Right token -> return token

      _ ->
        fail $ show txt <> " is not a valid JWT.Token"
