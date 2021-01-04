module Fission.Web.API.IPFS.Pin.Destroy.Types (Destroy) where

import qualified Network.IPFS.CID.Types     as IPFS

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types as Auth

type Destroy
  =  Auth.HigherOrder
  --
  :> Summary "Unpin CID"
  :> Description "DEPRECATED ⛔ Unpin an otherwise unassociated CID"
  --
  :> Capture "cid" IPFS.CID
  :> DeleteAccepted '[PlainText, OctetStream] NoContent

