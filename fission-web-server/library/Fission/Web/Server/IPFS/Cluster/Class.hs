module Fission.Web.Server.IPFS.Cluster.Class (MonadIPFSCluster (..)) where

import           Fission.Prelude

import qualified Network.IPFS.Types                          as IPFS

import qualified Fission.Web.Server.IPFS.Cluster.Error.Types as Cluster

-- | A monad giving access an IPFS Cluster Daemon
class Monad m => MonadIPFSCluster m where
  pin :: IPFS.CID -> m (Either Cluster.Error ())
