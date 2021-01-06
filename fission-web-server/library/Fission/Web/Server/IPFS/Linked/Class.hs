module Fission.Web.Server.IPFS.Linked.Class (MonadLinkedIPFS (..)) where

import qualified Network.IPFS.Types as IPFS

import           Fission.Prelude

-- | A monad representing when you have a two-way dependent external IPFS node
class Monad m => MonadLinkedIPFS m where
  getLinkedPeers :: m (NonEmpty IPFS.Peer)
