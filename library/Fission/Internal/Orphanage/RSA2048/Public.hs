{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.RSA2048.Public () where

import qualified Crypto.PubKey.RSA as RSA
import qualified System.IO.Unsafe  as Unsafe

import           Fission.Prelude

-- NOTE TO SELF
-- Quickcheck taking forever? Use RSA2048/Pair/Types.hs!
-- Anything over 256 bits takes _forever_
instance Arbitrary RSA.PublicKey where
  arbitrary = do
    exp <- elements [3, 5, 17, 257, 65537]
    return . fst . Unsafe.unsafePerformIO $ RSA.generate 2048 exp
