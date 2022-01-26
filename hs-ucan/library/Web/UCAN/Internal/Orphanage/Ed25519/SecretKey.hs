{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.UCAN.Internal.Orphanage.Ed25519.SecretKey () where

import qualified Crypto.PubKey.Ed25519 as Ed25519
import           RIO
import qualified System.IO.Unsafe      as Unsafe
import           Test.QuickCheck

instance Arbitrary Ed25519.SecretKey where
  arbitrary = return . Unsafe.unsafePerformIO $ Ed25519.generateSecretKey
