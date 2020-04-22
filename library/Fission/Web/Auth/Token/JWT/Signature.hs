module Fission.Web.Auth.Token.JWT.Signature
  ( parse
    -- * Reexport
  , module Fission.Web.Auth.Token.JWT.Signature.Types
  ) where

import qualified RIO.ByteString.Lazy as Lazy

import           Fission.Prelude

import           Fission.Key.Asymmetric.Algorithm.Types     as Algorithm
import           Fission.Web.Auth.Token.JWT.Signature.Types as Signature

-- Reexport

import           Fission.Web.Auth.Token.JWT.Signature.Types

parse :: Algorithm -> Lazy.ByteString -> Either String Signature
parse alg lazyBS =
  case alg of
    Algorithm.RSA2048 -> Signature.RS256   <$> eitherDecode lazyBS
    Algorithm.Ed25519 -> Signature.Ed25519 <$> eitherDecode lazyBS
