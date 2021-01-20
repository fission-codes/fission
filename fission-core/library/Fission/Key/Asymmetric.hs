-- | Top-level assymmetric-key module

module Fission.Key.Asymmetric
  ( signWith
  -- * Reexport
  , module Fission.Key.Asymmetric.Public.Types
  ) where

import qualified Crypto.PubKey.Ed25519               as Ed25519

import           Fission.Prelude

import           Fission.Key.Asymmetric.Public.Types

signWith :: Ed25519.SecretKey -> ByteString -> Ed25519.Signature
signWith sk bs = Ed25519.sign sk (Ed25519.toPublic sk) bs
