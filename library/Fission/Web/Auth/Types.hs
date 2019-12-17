-- | Authorization types; primarily more semantic aliases
module Fission.Web.Auth.Types
  ( ExistingUser
  , HerokuAddOnAPI
  ) where

import Database.Esqueleto
import Servant (BasicAuth)

import Fission.Prelude
import Fission.Models

-- | Authorization check for a normal user
type ExistingUser = BasicAuth "existing user" (Entity User)

-- | Authorization check for the Heroku Addon API
type HerokuAddOnAPI = BasicAuth "heroku add-on api" ByteString
