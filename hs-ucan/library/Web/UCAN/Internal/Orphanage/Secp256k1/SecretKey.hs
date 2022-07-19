{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.UCAN.Internal.Orphanage.Secp256k1.SecretKey () where

import qualified Crypto.Secp256k1      as Secp256k1
import           RIO
import qualified System.IO.Unsafe      as Unsafe
import           Test.QuickCheck

import           OpenSSL.Random

instance Arbitrary Secp256k1.SecKey where
  arbitrary = return . Unsafe.unsafePerformIO $ do
    bs <- randBytes 32
    maybe discard pure (Secp256k1.importSecKey bs)
