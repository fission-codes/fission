-- | Top-level assymmetric-key module

module Crypto.Key.Asymmetric
  ( signWith
  -- * Reexport
  , module Crypto.Key.Asymmetric.Public.Types
  ) where

import           Crypto.Key.Asymmetric.Public.Types
import qualified Crypto.PubKey.Ed25519              as Ed25519
import           RIO

signWith :: Ed25519.SecretKey -> ByteString -> Ed25519.Signature
signWith sk bs = Ed25519.sign sk (Ed25519.toPublic sk) bs
