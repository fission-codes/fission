module Fission.Web.Server.IPFS.Cluster.Class (MonadIPFSCluster (..)) where

-- 🧱

import           RIO.NonEmpty

-- 🌐

import           Servant.API

import           Servant.Client
import qualified Servant.Client.Streaming as Stream

-- ⚛️

import           Fission.Prelude

class MonadIO m => MonadIPFSCluster m where
  runCluster :: ClientM a -> m (NonEmpty (Async (Either ClientError a)))

  streamCluster ::
    Eq a
    => Stream.ClientM (SourceIO a)
    -> m (NonEmpty ((Async (Either ClientError a)), TChan a))
