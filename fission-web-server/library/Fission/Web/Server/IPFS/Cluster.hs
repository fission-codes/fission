module Fission.Web.Server.IPFS.Cluster
  ( pin
  , unpin
  , module Fission.Web.Server.IPFS.Cluster.Class
  ) where

-- 🌐

import           Network.IPFS.CID.Types
import qualified Network.IPFS.Client                   as IPFS

import           Servant.Client

-- ⚛️

import           Fission.Prelude

import           Fission.Web.Async
import           Fission.Web.Server.IPFS.Cluster.Class

pin :: MonadIPFSCluster m => CID -> m (Either ClientError ())
pin (CID hash) = do
  asyncRefs <- runCluster $ IPFS.pin hash
  waitAnySuccessCatch asyncRefs >>= \case
    Left err -> return $ Left err
    Right _  -> return $ Right ()

unpin :: MonadIPFSCluster m => CID -> m (Either ClientError ())
unpin (CID hash) = do
  asyncRefs <- runCluster $ IPFS.unpin hash True -- Recursive flag
  waitAnySuccessCatch asyncRefs >>= \case
    Left err -> return $ Left err
    Right _  -> return $ Right ()
