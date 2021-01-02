module Fission.IPFS.Linked.Class (MonadLinkedIPFS (..)) where

import           Fission.Prelude

import qualified Network.IPFS.Types as IPFS

-- | A monad representing when you have a two-way dependent external IPFS node
class Monad m => MonadLinkedIPFS m where
  getLinkedPeers :: m (NonEmpty IPFS.Peer)
