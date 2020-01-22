module Fission.Internal.Mock.Effect.Types
  ( RunDB          (..)
  , GetLinkedPeers (..)
  , GetVerifier    (..)
  , CheckTime      (..)
  , UpdateRoute53  (..)
  , SetDNSLink     (..)
  ) where

import           Fission.Prelude
import qualified Network.IPFS.Types as IPFS

data RunDB = RunDB
  deriving (Eq, Show)

newtype GetLinkedPeers = GetLinkedPeers (NonEmpty IPFS.Peer)
  deriving (Eq, Show)

data GetVerifier = GetVerifier
  deriving (Eq, Show)

data CheckTime = CheckTime
  deriving (Eq, Show)

data UpdateRoute53 = UpdateRoute53
  deriving (Eq, Show)

data SetDNSLink = SetDNSLink
  deriving (Eq, Show)
