module Fission.Web.Auth.JWT.Types
  ( JWT (..)

  -- * reexports
 
  , module Fission.Web.Auth.JWT.Claims.Types
  , module Fission.Web.Auth.JWT.Header.Types
  ) where

import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified RIO.ByteString.Lazy        as Lazy

import           Fission.Prelude
 
import           Fission.Key.Asymmetric.Algorithm.Types as Algorithm

import           Fission.Web.Auth.JWT.Claims.Types
import           Fission.Web.Auth.JWT.Header.Types
 
import           Fission.Web.Auth.JWT.Signature as Signature

-- | An RFC 7519 extended with support for Ed25519 keys,
--    and some specifics (claims, etc) for Fission's use case
data JWT = JWT
  { header :: !Header
  , claims :: !Claims
  , sig    :: !Signature
  } deriving (Show, Eq)

instance ToJSON JWT where
  toJSON JWT {..} = String encoded
    where
      encoded :: Text
      encoded = decodeUtf8Lenient (toStrictBytes payload) <> signed

      signed :: Text
      signed = utf8BuilderToText $ displayShow sig

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
