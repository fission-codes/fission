module Web.Ucan.Signature
  ( parse
    -- * Reexport
  , module Web.Ucan.Signature.Types
  ) where

import           Data.Aeson
import qualified Data.Aeson.Types                      as JSON

import           RIO

import           Crypto.Key.Asymmetric.Algorithm.Types as Algorithm
import           Web.Ucan.Signature.Types              as Signature

-- Reexport

import           Web.Ucan.Signature.Types

parse :: Algorithm -> JSON.Value -> JSON.Parser Signature
parse Algorithm.RSA2048 val = Signature.RS256   <$> parseJSON val
parse Algorithm.Ed25519 val = Signature.Ed25519 <$> parseJSON val
