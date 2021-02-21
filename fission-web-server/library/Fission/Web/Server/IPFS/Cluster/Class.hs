module Fission.Web.Server.IPFS.Cluster.Class (MonadIPFSCluster (..)) where

import           RIO.NonEmpty
import           Servant.Client

import           Fission.Prelude

class MonadIO m => MonadIPFSCluster m where
  runCluster :: ClientM a -> m (NonEmpty (Async (Either ClientError a)))
