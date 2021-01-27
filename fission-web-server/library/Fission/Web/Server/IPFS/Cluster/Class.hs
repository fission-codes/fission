module Fission.Web.Server.IPFS.Cluster.Class (MonadIPFSCluster (..)) where

import           Fission.Prelude

import qualified Network.IPFS.Types                          as IPFS

import qualified Fission.Web.Server.IPFS.Cluster.Error.Types as Cluster

-- | A monad representing the ability to pin a CID, either to an IPFS node or a cluster
class Monad m => MonadIPFSCluster m where
  pin :: IPFS.CID -> m (Either Cluster.Error ())
