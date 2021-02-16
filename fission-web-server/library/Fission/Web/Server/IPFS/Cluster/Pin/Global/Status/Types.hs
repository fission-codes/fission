module Fission.Web.Server.IPFS.Cluster.Pin.Global.Status.Types (GlobalPinStatus (..)) where

import           Fission.Prelude

import           Fission.Web.Server.IPFS.Cluster.Pin.Status.Types as Pin

newtype GlobalPinStatus = GlobalPinStatus { pinStatus :: Pin.Status }
  deriving (Eq, Show)

-- >>> decodeEither "{\"cid\":{\"/\":\"QmZtmD2qt6fJot32nabSP3CUjicnypEBz7bHVDhPQt9aAy\"},\"peer_map\":{\"QmZ55vfp7dW4pUBctho1Di3xB3sdwtjvqtwZ5NtfWBeMiL\":{\"cid\":{\"/\":\"QmZtmD2qt6fJot32nabSP3CUjicnypEBz7bHVDhPQt9aAy\"},\"peer\":\"QmZ55vfp7dW4pUBctho1Di3xB3sdwtjvqtwZ5NtfWBeMiL\",\"peername\":\"production-ipfs-cluster-us-east-1-node1\",\"status\":\"unpinned\",\"timestamp\":\"2021-02-16T22:20:01.293178714Z\",\"error\":\"\"},\"QmZD3iWoPcCCuC7TzTRHAdwrv47mnkk1Tj3BaivEnkm5DG\":{\"cid\":{\"/\":\"QmZtmD2qt6fJot32nabSP3CUjicnypEBz7bHVDhPQt9aAy\"},\"peer\":\"QmZD3iWoPcCCuC7TzTRHAdwrv47mnkk1Tj3BaivEnkm5DG\",\"peername\":\"production-ipfs-cluster-us-east-1-node0\",\"status\":\"unpinned\",\"timestamp\":\"2021-02-16T22:20:01.292175673Z\",\"error\":\"\"},\"QmbzBUTTdsWSYf3Xt4NCryDKFMcKPcptPePkPwbqQvHk7j\":{\"cid\":{\"/\":\"QmZtmD2qt6fJot32nabSP3CUjicnypEBz7bHVDhPQt9aAy\"},\"peer\":\"QmbzBUTTdsWSYf3Xt4NCryDKFMcKPcptPePkPwbqQvHk7j\",\"peername\":\"production-ipfs-cluster-eu-north-1-node1\",\"status\":\"unpinned\",\"timestamp\":\"2021-02-16T22:20:01.352443413Z\",\"error\":\"\"},\"Qmdb63TsHtV9m1sJfMui7ZLj92ovBWCu47k4Qyp16AMWV8\":{\"cid\":{\"/\":\"QmZtmD2qt6fJot32nabSP3CUjicnypEBz7bHVDhPQt9aAy\"},\"peer\":\"Qmdb63TsHtV9m1sJfMui7ZLj92ovBWCu47k4Qyp16AMWV8\",\"peername\":\"production-ipfs-cluster-us-east-1-node2\",\"status\":\"unpinned\",\"timestamp\":\"2021-02-16T22:20:01.292930581Z\",\"error\":\"\"},\"QmeY2hDea6NLfupt5gkbUnvifBy68aZ6wSTUTmpufdFtbh\":{\"cid\":{\"/\":\"QmZtmD2qt6fJot32nabSP3CUjicnypEBz7bHVDhPQt9aAy\"},\"peer\":\"QmeY2hDea6NLfupt5gkbUnvifBy68aZ6wSTUTmpufdFtbh\",\"peername\":\"production-ipfs-cluster-eu-north-1-node0\",\"status\":\"unpinned\",\"timestamp\":\"2021-02-16T22:20:01.344630765Z\",\"error\":\"\"}}}"
-- Right (GlobalPinStatus Pinned)
instance FromJSON GlobalPinStatus where
  parseJSON = withObject "GlobalPinInfo" \obj -> do
    statusMap :: Map Text Pin.Status <- obj .: "peer_map"
    return GlobalPinStatus {pinStatus = foldr minRachet Pinned statusMap}

    where
      minRachet :: Pin.Status -> Pin.Status -> Pin.Status
      minRachet status acc =
        case (status, acc) of
          -- Failure overrides all else
          (FailedWith err, _)   -> FailedWith err
          (_, FailedWith err)   -> FailedWith err

          -- Inconsistent is a kind of failure
          -- i.e. inconsistent state, because of how IPFS Cluster is set up
          (Inconsistent err, _) -> Inconsistent err
          (_, Inconsistent err) -> Inconsistent err

          -- They must all be queued for this to be true
          (Queued, _)           -> Queued
          (_, Queued)           -> Queued

          -- Found route, so process started. Wait for all to report pinned.
          (Pinning, _)          -> Pinning
          (_, Pinning)          -> Pinning

          -- Done, but drop to any lower level if waiting.
          -- This is the most conservative strategy, and we can flip this if it's
          -- turning out to be a bottleneck for sme reason.
          (Pinned, Pinned)      -> Pinned
