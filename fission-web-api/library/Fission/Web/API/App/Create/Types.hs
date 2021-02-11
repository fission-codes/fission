module Fission.Web.API.App.Create.Types (Create) where

import qualified Fission.App.Name.Types     as App
import           Fission.URL.Types

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types as Auth

type Create
  =  Summary "Create app"
  :> Description "Creates a new app, assigns an initial subdomain, and sets an asset placeholder"
  --
  :> QueryParam "subdomain" App.Name
  --
  :> Auth.HigherOrder
  :> PostAccepted '[JSON] URL
