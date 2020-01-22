{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.Peer () where

import           Data.List.NonEmpty  as NonEmpty
import qualified RIO.ByteString.Lazy as Lazy
import           Servant

import qualified Network.IPFS.Types  as IPFS

import           Fission.Prelude

instance Arbitrary IPFS.Peer where
  arbitrary = IPFS.Peer <$> arbitrary
