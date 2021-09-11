module Fission.Web.API.IPFS.Pin.Types (Routes (..)) where

import           Network.IPFS.CID.Types     as IPFS

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types as Auth

data Routes mode = Routes
  { pin ::
      mode
      :- Summary "Pin CID"
      :> Description "DEPRECATED ⛔ Pin an otherwise unassociated CID"
      --
      :> Capture "cid" IPFS.CID
      --
      :> Auth.HigherOrder
      :> Put '[PlainText, OctetStream] NoContent

  , unpin ::
      mode
      :- Summary "Unpin CID"
      :> Description "DEPRECATED ⛔ Unpin an otherwise unassociated CID"
      --
      :> Capture "cid" IPFS.CID
      --
      :> Auth.HigherOrder
      :> DeleteAccepted '[PlainText, OctetStream] NoContent
  }
  deriving Generic
