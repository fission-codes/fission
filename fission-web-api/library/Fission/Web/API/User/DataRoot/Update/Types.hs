module Fission.Web.API.User.DataRoot.Update.Types (UpdateRoot) where

import qualified Network.IPFS.CID.Types     as IPFS

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types as Auth

type UpdateRoot
  =  Summary "Update data root"
  :> Description "Set/update currently authenticated user's file system content"
  --
  :> Capture "newCID" IPFS.CID
  --
  :> Auth.HigherOrder
  :> PatchNoContent
