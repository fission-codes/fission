module Web.UCAN.Signature
  ( parse
    -- * Reexport
  , module Web.UCAN.Signature.Types
  ) where

import           Data.Aeson
import qualified Data.Aeson.Types                      as JSON

import           RIO

import           Crypto.Key.Asymmetric.Algorithm.Types as Algorithm
import           Web.UCAN.Signature.Types              as Signature

-- Reexport

import           Web.UCAN.Signature.Types

parse :: Algorithm -> JSON.Value -> JSON.Parser Signature
parse Algorithm.RSA2048   val = Signature.RS256     <$> parseJSON val
parse Algorithm.Ed25519   val = Signature.Ed25519   <$> parseJSON val
parse Algorithm.Secp256k1 val = Signature.Secp256k1 <$> parseJSON val
