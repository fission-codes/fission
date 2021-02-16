module Fission.Web.Server.IPFS.Cluster.Pin.Status.Types (Status (..)) where

import           Fission.Prelude

data Status
  = FailedWith   Text -- ^ An actual error as reported by IPFS Cluster
  | Inconsistent Text -- ^ Not a pinning-related sttatus (because of how IPFS Cluster is set up)
  | Queued            -- ^ Knows about the CID, but not how to get it (yet)
  | Pinning           -- ^ Has a route to a peer with that CID
  | Pinned            -- ^ IPFS node has that CID
  deriving (Eq, Show)

instance FromJSON Status where
  parseJSON = withObject "Cluster.Pin.Status" \obj -> do
    status   <- obj .: "status"
    errorTxt <- obj .: "error"

    case status :: Text of
      "pinned"       -> return Pinned

      "pinning"      -> return Pinning

      "pin_queued"   -> return Queued
      "queued"       -> return Queued

      "unpinned"     -> return $ Inconsistent "unpinned"
      "unpin_queued" -> return $ Inconsistent "unpin_queued"
      "remote"       -> return $ Inconsistent "remote"

      _              -> return $ FailedWith errorTxt
