module Fission.Web.API.App.Types (Routes (..)) where

import qualified Network.IPFS.CID.Types                  as IPFS

import qualified Fission.App.Name.Types                  as App
import           Fission.URL.Types

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.App.Destroy.Types       as Destroy
import           Fission.Web.API.App.Index.Payload.Types
import qualified Fission.Web.API.Auth.Types              as Auth

data Routes mode = Routes
  { index ::
      mode
      :- Summary "App index"
      :> Description "A list of all of your apps and their associated domain names"
      --
      :> Auth.HigherOrder
      :> Get '[JSON] (Map Natural Payload)

  , create ::
      mode
      :- Summary "Create app"
      :> Description "Creates a new app, assigns an initial subdomain, and sets an asset placeholder"
      --
      :> QueryParam "subdomain" App.Name
      --
      :> Auth.HigherOrder
      :> PostAccepted '[JSON] URL

  , update ::
      mode
      :- Summary     "Set app content"
      :> Description "Update the content (CID) for an app"
      --
      :> Capture    "App URL"   URL
      :> Capture    "New CID"   IPFS.CID
      :> QueryParam "copy-data" Bool
      --
      :> Auth.HigherOrder
      :> PatchAccepted '[JSON] ()

  , destroy ::
      mode
      :- ToServantApi Destroy.Routes
  }
  deriving Generic
