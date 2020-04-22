module Fission.Web.Client.IPFS
  ( DAGPut
  , Unpin
  , Pin
  , Upload
  , CIDs
  ) where

import           Servant
import           Fission.Web.Routes (IPFSPrefix)

import qualified Fission.Web.IPFS        as IPFS
import qualified Fission.Web.IPFS.CID    as CID
import qualified Fission.Web.IPFS.Upload as Upload
import qualified Fission.Web.IPFS.Pin    as Pin
import qualified Fission.Web.IPFS.DAG    as DAG

type AuthedIPFS more
  = IPFS.Auth
    :> IPFSPrefix
    :> more

type DAGPut = AuthedIPFS ("dag" :> DAG.API)
type Pin    = AuthedIPFS Pin.PinAPI
type Unpin  = AuthedIPFS Pin.UnpinAPI
type Upload = AuthedIPFS Upload.API
type CIDs   = AuthedIPFS ("cids" :> CID.API)
