module Fission.Web.API.IPFS.Pin.Create.Types (Create) where

import qualified Network.IPFS.CID.Types     as IPFS

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types as Auth

type Create
  =  Summary "Pin CID"
  :> Description "DEPRECATED ⛔ Pin an otherwise unassociated CID"
  --
  :> Capture "cid" IPFS.CID
  --
  :> Auth.HigherOrder
  :> Put '[PlainText, OctetStream] NoContent
