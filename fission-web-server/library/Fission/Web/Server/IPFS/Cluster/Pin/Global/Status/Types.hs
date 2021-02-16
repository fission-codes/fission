module Fission.Web.Server.IPFS.Cluster.Pin.Global.Status.Types (GlobalPinStatus (..)) where

import           Fission.Prelude

import           Fission.Web.Server.IPFS.Cluster.Pin.Status.Types as Pin

newtype GlobalPinStatus = GlobalPinStatus { pinStatus :: Pin.Status }
  deriving (Eq, Show)

instance FromJSON GlobalPinStatus where
  parseJSON = withObject "GlobalPinInfo" \obj -> do
    statusMap :: Map Text Pin.Status <- obj .: "peer_map"
    return GlobalPinStatus {pinStatus = foldr minRachet Pinned statusMap}

    where
      minRachet :: Pin.Status -> Pin.Status -> Pin.Status
      minRachet status acc =
        case (status, acc) of
          -- Failure overrides all else
          (FailedWith err,   _             )   -> FailedWith err
          (_,                FailedWith err)   -> FailedWith err

          -- Inconsistent is a kind of failure
          -- i.e. inconsistent state, because of how IPFS Cluster is set up
          (Inconsistent err, _               ) -> Inconsistent err
          (_,                Inconsistent err) -> Inconsistent err

          -- They must all be queued for this to be true
          (Queued,           _     )           -> Queued
          (_,                Queued)           -> Queued

          -- Found route, so process started. Wait for all to report pinned.
          (Pinning,          _)                -> Pinning

          -- Done, but drop to any lower level if waiting.
          -- This is the most conservative strategy, and we can flip this if it's
          -- turning out to be a bottleneck for sme reason.
          (Pinned,           Pinned)           -> Pinned
          (Pinned,           other )           -> other
