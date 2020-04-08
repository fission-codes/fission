module Fission.Web.Auth.JWT.Signature.Ed25519 (sign) where

import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified RIO.ByteString.Lazy   as Lazy

import           Fission.Prelude
import qualified Fission.Key.Store as Key

import qualified Fission.Web.Auth.JWT.Header.Types    as JWT
import qualified Fission.Web.Auth.JWT.Claims.Types    as JWT
import qualified Fission.Web.Auth.JWT.Signature.Types as JWT.Sig

sign :: JWT.Header -> JWT.Claims -> Ed25519.SecretKey -> JWT.Sig.Signature
sign header claims sk =
  JWT.Sig.Ed25519 .  Key.signWith sk . Lazy.toStrict $
    encode header <> "." <> encode claims
