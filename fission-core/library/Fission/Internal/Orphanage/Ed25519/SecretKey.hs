{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.Ed25519.SecretKey () where

import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified System.IO.Unsafe      as Unsafe

import           Fission.Prelude

instance Arbitrary Ed25519.SecretKey where
  arbitrary = return . Unsafe.unsafePerformIO $ Ed25519.generateSecretKey
