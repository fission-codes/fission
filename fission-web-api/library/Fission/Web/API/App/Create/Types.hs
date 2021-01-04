module Fission.Web.API.App.Create.Types (Create) where

import           Fission.URL

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types as Auth

type Create
  =  Auth.HigherOrder
  --
  :> Summary "Create app"
  :> Description "Creates a new app, assigns an initial subdomain, and sets an asset placeholder"
  --
  :> QueryParam "subdomain" Subdomain
  --
  :> PostAccepted '[JSON] URL
