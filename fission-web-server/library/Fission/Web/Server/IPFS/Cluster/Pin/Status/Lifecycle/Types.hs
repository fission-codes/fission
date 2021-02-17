module Fission.Web.Server.IPFS.Cluster.Pin.Status.Lifecycle.Types (Lifecycle (..)) where

import           Fission.Prelude

data Lifecycle
  = Queued      -- ^ Knows about the CID, but not how to get it (yet)
  | Pinning     -- ^ Has a route to a peer with that CID
  | PinComplete -- ^ IPFS node has that CID
  deriving (Show, Eq, Ord)

instance FromJSON Lifecycle where
  parseJSON = withText "Lifecycle" \txt ->
    case txt of
      "pinned"     -> return PinComplete

      "pinning"    -> return Pinning

      "pin_queued" -> return Queued
      "queued"     -> return Queued
      _            -> fail "Not a valid Cluster.Pin.Lifecycle"
