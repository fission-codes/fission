{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.Serilaized () where

import           RIO
import           Network.IPFS.File.Types as File

import           Test.QuickCheck
import           Test.QuickCheck.Instances.ByteString ()

instance Arbitrary File.Serialized where
  arbitrary = File.Serialized <$> arbitrary
