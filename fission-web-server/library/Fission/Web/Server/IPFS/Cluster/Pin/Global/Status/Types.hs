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
        case (acc, status) of
          (FailedWith err, _)              -> FailedWith err
          (_,              FailedWith err) -> FailedWith err
          (Pinned,         Pinned)         -> Pinned
          (Pinned,         other)          -> other
          _                                -> Pinning
