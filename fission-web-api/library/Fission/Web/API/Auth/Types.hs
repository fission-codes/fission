module Fission.Web.API.Auth.Types
  ( HerokuAddOnAPI
  , RegisterDID
  , HigherOrder
  ) where

import           Servant.API                       (BasicAuth)
import           Servant.API.Experimental.Auth

-- FIXME
import           Fission.Web.Auth.Token.Types
import           Servant.Client.Core

import           Fission.Web.API.Heroku.Auth.Types as Heroku

-- | Authorization check for the Heroku Addon API
type HerokuAddOnAPI = BasicAuth "heroku add-on api" Heroku.Auth

-- | Authorization check to return encoded DID for registering new users
type RegisterDID = AuthProtect "register-did"

-- FIXME
-- type instance AuthServerData (AuthProtect "register-did") = DID
type instance AuthClientData (AuthProtect "register-did") = Token

-- | Higher order auth that encompasses Basic & JWT auth
type HigherOrder = AuthProtect "higher-order"

-- FIXME
-- type instance AuthServerData (AuthProtect "higher-order") = Authorization
type instance AuthClientData (AuthProtect "higher-order") = Token
