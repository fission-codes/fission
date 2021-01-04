module Fission.Web.API.App.Update.Types (Update) where

import qualified Network.IPFS.CID.Types     as IPFS

import           Fission.URL.Types

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types as Auth

type Update
  =  Auth.HigherOrder
  --
  :> Summary     "Set app content"
  :> Description "Update the content (CID) for an app"
  --
  :> Capture     "App URL"   URL
  :> Capture     "New CID"   IPFS.CID
  :> QueryParam  "copy-data" Bool
  --
  :> PatchAccepted '[JSON] NoContent
