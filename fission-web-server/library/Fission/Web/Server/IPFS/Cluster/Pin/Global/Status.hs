module Fission.Web.Server.IPFS.Cluster.Pin.Global.Status
  ( progress
  , aggregateError
  -- * reexports
  , module Fission.Web.Server.IPFS.Cluster.Pin.Global.Status.Types
  , module Fission.Web.Server.IPFS.Cluster.Pin.Status.Types
  ) where

import qualified RIO.List                                                as List

import           Fission.Prelude

import           Fission.Web.Server.IPFS.Cluster.Pin.Global.Status.Types
import           Fission.Web.Server.IPFS.Cluster.Pin.Status.Types

progress :: GlobalPinStatus -> Status
progress status@GlobalPinStatus {..} =
  case List.maximumMaybe lifecycles of
    Nothing -> Failed $ aggregateError status
    Just lc -> Normal lc

aggregateError :: GlobalPinStatus -> Error
aggregateError GlobalPinStatus {errs} =
  case errs of
    []                     -> FailedWith   "Failed to pin or return message. Is cluster empty?"
    (FailedWith   txt : _) -> FailedWith   txt
    (Inconsistent txt : _) -> Inconsistent txt
