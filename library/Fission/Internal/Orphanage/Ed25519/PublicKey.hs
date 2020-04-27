{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.Ed25519.PublicKey () where

import qualified Crypto.PubKey.Ed25519 as Ed25519

import           Fission.Prelude
import           Fission.Internal.Orphanage.Ed25519.SecretKey ()

instance Arbitrary Ed25519.PublicKey where
  arbitrary = Ed25519.toPublic <$> arbitrary
