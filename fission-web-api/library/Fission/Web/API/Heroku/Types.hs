module Fission.Web.API.Heroku.Types (Heroku) where

import           Fission.Web.API.Prelude

import           Fission.Web.API.Heroku.Deprovision.Types
import           Fission.Web.API.Heroku.Provision.Types

-- | Top level Heroku entry point
type Heroku = "heroku" :> "resources" :> API

-- | Internal Heroku web API
type API = Provision :<|> Deprovision
