{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.UCAN.Internal.Orphanage.Ed25519.SecretKey () where

import qualified Crypto.PubKey.Ed25519 as Ed25519
import           RIO
import qualified System.IO.Unsafe      as Unsafe
import           Test.QuickCheck

instance Arbitrary Ed25519.SecretKey where
  -- introducing a lambda to make sure we're calling generateSecretKey more than once!
  arbitrary = sized (\_ -> return . Unsafe.unsafePerformIO $ Ed25519.generateSecretKey)
  {-# NOINLINE arbitrary #-}
