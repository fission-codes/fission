{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.RSA2048 () where

import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified System.IO.Unsafe      as Unsafe

import           Fission.Prelude


-- Wait. This is weird. How did this end up in this module?
instance Arbitrary Ed25519.SecretKey where
  arbitrary = return . Unsafe.unsafePerformIO $ Ed25519.generateSecretKey
