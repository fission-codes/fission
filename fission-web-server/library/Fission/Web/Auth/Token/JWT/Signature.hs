module Fission.Web.Auth.Token.JWT.Signature
  ( parse
    -- * Reexport
  , module Fission.Web.Auth.Token.JWT.Signature.Types
  ) where

import qualified Data.Aeson.Types as JSON

import           Fission.Prelude

import           Fission.Key.Asymmetric.Algorithm.Types     as Algorithm
import           Fission.Web.Auth.Token.JWT.Signature.Types as Signature

-- Reexport

import           Fission.Web.Auth.Token.JWT.Signature.Types

parse :: Algorithm -> JSON.Value -> JSON.Parser Signature
parse Algorithm.RSA2048 val = Signature.RS256   <$> parseJSON val
parse Algorithm.Ed25519 val = Signature.Ed25519 <$> parseJSON val
