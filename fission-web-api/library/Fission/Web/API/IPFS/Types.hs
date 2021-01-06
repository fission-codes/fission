module Fission.Web.API.IPFS.Types (IPFS) where

import           Fission.Web.API.Prelude

import           Fission.Web.API.IPFS.CID.Types
import           Fission.Web.API.IPFS.DAG.Types
import           Fission.Web.API.IPFS.Download.Types
import           Fission.Web.API.IPFS.Peer.Types
import           Fission.Web.API.IPFS.Pin.Types
import           Fission.Web.API.IPFS.Upload.Types

-- | Initial IPFS entrypoint
type IPFS = "ipfs" :> API

-- | Internal IPFS routes
type API
  =    CID
  :<|> DAG
  :<|> Peer
  :<|> Upload
  :<|> Pin
  :<|> Download
