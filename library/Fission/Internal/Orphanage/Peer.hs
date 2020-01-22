{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.Peer () where

import qualified Network.IPFS.Types  as IPFS

import           Fission.Prelude

instance Arbitrary IPFS.Peer where
  arbitrary = IPFS.Peer <$> arbitrary
