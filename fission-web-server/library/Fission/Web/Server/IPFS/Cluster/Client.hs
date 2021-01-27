module Fission.Web.Server.IPFS.Cluster.Client
  ( API
  , pinClient
  ) where

import           Fission.Prelude

import           Servant
import           Servant.Client

import qualified Network.IPFS.CID.Types as IPFS

type API
  = "pins"
  :> Capture "cid" IPFS.CID
  :> PostNoContent

pinClient :: IPFS.CID -> ClientM NoContent
pinClient = client $ Proxy @API
