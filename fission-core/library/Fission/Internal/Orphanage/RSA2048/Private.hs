{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.RSA2048.Private () where

import qualified Crypto.PubKey.RSA as RSA
import qualified System.IO.Unsafe  as Unsafe

import           Fission.Prelude

instance Arbitrary RSA.PrivateKey where
  arbitrary = do
    exp <- elements [3, 5, 17, 257, 65537]
    return . snd . Unsafe.unsafePerformIO $ RSA.generate 2048 exp
