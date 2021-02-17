module Fission.Web.Server.IPFS.Cluster.Pin.Global.Status.Types (GlobalPinStatus (..)) where

import           Fission.Prelude

import           Fission.Web.Server.IPFS.Cluster.Pin.Status.Types as Pin

-- Adjust name
data GlobalPinStatus = GlobalPinStatus
  { lifecycles :: [Pin.Lifecycle]
  , errs       :: [Pin.Error]
  }
  deriving (Eq, Show)

instance Semigroup GlobalPinStatus where
  a <> b =
    GlobalPinStatus { lifecycles = lifecycles a <> lifecycles b
                    , errs       = errs       a <> errs       b
                    }

instance Monoid GlobalPinStatus where
  mempty = GlobalPinStatus [] []

instance FromJSON GlobalPinStatus where
  parseJSON = withObject "GlobalPinStatus" \obj -> do
    statusMap :: Map Text Pin.Status <- obj .: "peer_map"
    return $ foldr splitter mempty statusMap

    where
      splitter :: Pin.Status -> GlobalPinStatus -> GlobalPinStatus
      splitter (Normal lc)  acc@(GlobalPinStatus {..}) = acc { lifecycles = (lc  : lifecycles) }
      splitter (Failed err) acc@(GlobalPinStatus {..}) = acc { errs       = (err : errs)       }
