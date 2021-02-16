module Fission.Web.Server.IPFS.Cluster.Client
  ( API
  , pinClient
  ) where

import           Fission.Prelude

import           Servant
import           Servant.Client


import qualified Fission.Web.Server.IPFS.Cluster.Types as Cluster
import qualified Network.IPFS.CID.Types                as IPFS

type API
  =    PinAPI
  :<|> StatusAPI

type PinAPI
  = "pins"
  :> Capture "cid" IPFS.CID
  :> PostNoContent

type StatusAPI
  = "pins"
  :> Capture "cid" IPFS.CID
  :> '[JSON] Cluster.StatusResp

pinClient    :: IPFS.CID -> ClientM NoContent
statusClient :: IPFS.CID -> ClientM Clsuter.StatusResp
pinClient :<|> statusClietn= client $ Proxy @API
