-- | Authorization types; primarily more semantic aliases
module Fission.Web.Auth.Types
  ( HerokuAddOnAPI
  , RegisterDID
  , HigherOrder
  , module Fission.Web.Auth.Token
  ) where

import Servant (BasicAuth)
import Servant.API.Experimental.Auth
import Servant.Server.Experimental.Auth
import Servant.Client.Core

import Fission.Platform.Heroku.Auth.Types as Heroku

import Fission.Web.Auth.Token
import Fission.User.DID.Types
import Fission.Authorization.Types

-- | Authorization check for the Heroku Addon API
type HerokuAddOnAPI = BasicAuth "heroku add-on api" Heroku.Auth

-- | Authorization check to return encoded DID for registering new users
type RegisterDID = AuthProtect "register-did"
type instance AuthServerData (AuthProtect "register-did") = DID
type instance AuthClientData (AuthProtect "register-did") = Token

-- | Higher order auth that encompasses Basic & JWT auth
type HigherOrder = AuthProtect "higher-order"
type instance AuthServerData (AuthProtect "higher-order") = Authorization
type instance AuthClientData (AuthProtect "higher-order") = Token
