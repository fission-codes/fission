module Fission.Web.Server.IPFS.Cluster
  ( pin
  , unpin
  , pinStream
  , module Fission.Web.Server.IPFS.Cluster.Class
  ) where

-- 🌐

import           Network.IPFS.CID.Types
import qualified Network.IPFS.Client                   as IPFS
import           Network.IPFS.Client.Pin               as Pin
import           Network.IPFS.Client.Streaming.Pin     as Pin

import           Servant.Client
import qualified Servant.Client.Streaming              as Streaming

-- ⚛️

import           Fission.Prelude

import           Fission.Web.Async
import           Fission.Web.Server.IPFS.Cluster.Class

pin :: MonadIPFSCluster m Pin.Response => CID -> m (Either ClientError Pin.Response)
pin cid = do
  asyncRefs <- runCluster $ IPFS.pin cid
  waitAnySuccessCatch asyncRefs >>= \case
    Left  err      -> return $ Left err
    Right (_, val) -> return $ Right val

unpin :: MonadIPFSCluster m Pin.Response => CID -> m (Either ClientError Pin.Response)
unpin cid = do
  asyncRefs <- runCluster $ IPFS.unpin cid True -- Recursive flag
  waitAnySuccessCatch asyncRefs >>= \case
    Left  err      -> return $ Left err
    Right (_, val) -> return $ Right val

pinStream :: MonadIPFSCluster m PinStatus => CID -> m (Either ClientError PinStatus)
pinStream cid = do
  pseudoStreams <- streamCluster $ (Streaming.client $ Proxy @PinComplete) cid (Just True)
  let asyncRefs = fst <$> pseudoStreams
  waitAnySuccessCatch asyncRefs >>= \case
    Left  err      -> return $ Left err
    Right (_, val) -> return $ Right val
