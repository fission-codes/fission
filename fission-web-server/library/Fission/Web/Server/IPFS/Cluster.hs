module Fission.Web.Server.IPFS.Cluster
  ( pin
  , unpin
  , pinStream
  , module Fission.Web.Server.IPFS.Cluster.Class
  ) where

-- 🌐

import           Network.IPFS.CID.Types
import qualified Network.IPFS.Client                                 as IPFS
import qualified Network.IPFS.Client.Pin                             as Pin

import           Servant.Client
import qualified Servant.Client.Streaming                            as Streaming

-- ⚛️

import           Fission.Prelude

import           Fission.Web.Async

import           Fission.Web.Server.IPFS.Cluster.Class
import           Fission.Web.Server.IPFS.Streaming.Pin.Types

import           Fission.Internal.Orphanage.IPFS.Client.Pin.Response ()

pin :: MonadIPFSCluster m Pin.Response => CID -> m (Either ClientError Pin.Response)
pin (CID hash) = do
  asyncRefs <- runCluster $ IPFS.pin hash
  waitAnySuccessCatch asyncRefs >>= \case
    Left  err      -> return $ Left err
    Right (_, val) -> return $ Right val

unpin :: MonadIPFSCluster m Pin.Response => CID -> m (Either ClientError Pin.Response)
unpin (CID hash) = do
  asyncRefs <- runCluster $ IPFS.unpin hash True -- Recursive flag
  waitAnySuccessCatch asyncRefs >>= \case
    Left  err      -> return $ Left err
    Right (_, val) -> return $ Right val

pinStream :: MonadIPFSCluster m PinStatus => CID -> m (Either ClientError PinStatus)
pinStream cid = do
  pseudoStreams <- streamCluster $ (Streaming.client $ Proxy @PinComplete) (Just cid) (Just True)
  let asyncRefs = fst <$> pseudoStreams
  waitAnySuccessCatch asyncRefs >>= \case
    Left  err      -> return $ Left err
    Right (_, val) -> return $ Right val
