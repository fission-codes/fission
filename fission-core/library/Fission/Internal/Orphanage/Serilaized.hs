{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.Serilaized () where

import           Network.IPFS.File.Types              as File
import           RIO

import           Test.QuickCheck
import           Test.QuickCheck.Instances.ByteString ()

instance Arbitrary File.Serialized where
  arbitrary = File.Serialized <$> arbitrary
