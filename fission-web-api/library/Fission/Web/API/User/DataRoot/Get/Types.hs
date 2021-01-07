module Fission.Web.API.User.DataRoot.Get.Types (GetRoot) where

import qualified Network.IPFS.CID.Types      as IPFS

import           Fission.User.Username.Types

import           Fission.Web.API.Prelude

type GetRoot
  =  Summary "Get Data Root"
  :> Description "Retrieve a user's data root from DNS"
  --
  :> Capture "username" Username
  :> Get '[JSON, PlainText] IPFS.CID
