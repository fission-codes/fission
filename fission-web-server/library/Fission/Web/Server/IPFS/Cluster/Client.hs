module Fission.Web.Server.IPFS.Cluster.Client
  ( pin
  , status
  , PinAPI
  , StatusAPI
  ) where

-- 🌐

import qualified Network.IPFS.CID.Types                                  as IPFS

import           Servant
import           Servant.Client

-- ⚛️

import           Fission.Prelude

import qualified Fission.Web.Server.IPFS.Cluster.Pin.Global.Status.Types as Cluster

-- 🔺

type PinAPI
  = "pins"
  :> Capture "cid" IPFS.CID
  :> PostNoContent

type StatusAPI
  = "pins"
  :> Capture "cid" IPFS.CID
  :> Get '[JSON] Cluster.GlobalPinStatus

-- ⚙️

pin :: IPFS.CID -> ClientM NoContent
pin = client $ Proxy @PinAPI

status :: IPFS.CID -> ClientM Cluster.GlobalPinStatus
status = client $ Proxy @StatusAPI
