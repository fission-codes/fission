-- FIXME remve thsi module
module Fission.Key.Sign.Class (Signer (..)) where

import           Crypto.Random

import           Fission.Prelude

class Signer secret where
  sign ::
    MonadRandom m
    => secret       -- ^ Shared secret or private key
    -> ByteString   -- ^ Message to sign
    -> m ByteString -- ^ Signature
