-- | Authorization types; primarily more semantic aliases
module Fission.Web.Auth.Types
  ( HerokuAddOnAPI
  , RegisterDid
  , HigherOrder
  , module Fission.Web.Auth.Token
  ) where

import Database.Esqueleto

import Servant (BasicAuth, BasicAuthData)
import Servant.API.Experimental.Auth
import Servant.Server.Experimental.Auth
import Servant.Client.Core

import Fission.Prelude
import Fission.Models
import Fission.Platform.Heroku.Auth.Types as Heroku

import Fission.User.DID.Types

import Fission.Web.Auth.Token

-- | Authorization check for the Heroku Addon API
type HerokuAddOnAPI = BasicAuth "heroku add-on api" Heroku.Auth

-- | Authorization check to return encoded did for registering new users
type RegisterDid = AuthProtect "register-did"
type instance AuthServerData (AuthProtect "register-did") = DID
type instance AuthClientData (AuthProtect "register-did") = ()

-- | Higher order auth that encompasses Basic & JWT auth
type HigherOrder = AuthProtect "higher-order"
type instance AuthServerData (AuthProtect "higher-order") = Entity User
type instance AuthClientData (AuthProtect "higher-order") = (Maybe BasicAuthData)
