module Fission.Web.Server.IPFS.Cluster.Pin.Status.Types
  ( Status (..)
  , module Fission.Web.Server.IPFS.Cluster.Pin.Status.Error
  , module Fission.Web.Server.IPFS.Cluster.Pin.Status.Lifecycle.Types
  ) where

import           Fission.Prelude

import           Fission.Web.Server.IPFS.Cluster.Pin.Status.Error
import           Fission.Web.Server.IPFS.Cluster.Pin.Status.Lifecycle.Types

data Status
  = Normal Lifecycle
  | Failed Error
  deriving (Show, Eq)

instance FromJSON Status where
  parseJSON = withObject "Cluster.Pin.Status" \obj -> do
    lifecycle <- obj .:? "status"
    pinError  <- obj .:  "error"

    case lifecycle of
      Just lc -> return $ Normal lc
      Nothing -> return $ Failed pinError
