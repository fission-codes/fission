-- | Authorization types; primarily more semantic aliases
module Fission.Web.Auth.Types
  ( ExistingUser
  , HerokuAddOnAPI
  ) where

import Database.Esqueleto
import Servant (BasicAuth)

import Fission.Models
import Fission.Platform.Heroku.Auth.Types as Heroku

-- | Authorization check for a normal user
type ExistingUser = BasicAuth "existing user" (Entity User)

-- | Authorization check for the Heroku Addon API
type HerokuAddOnAPI = BasicAuth "heroku add-on api" Heroku.Auth
