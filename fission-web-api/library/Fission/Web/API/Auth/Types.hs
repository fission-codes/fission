module Fission.Web.API.Auth.Types
  ( HerokuAddOnAPI
  , RegisterDID
  , HigherOrder
  , Auth
  ) where

import           Servant.API.Experimental.Auth
import           Servant.Client.Core

import           Fission.Web.Auth.Token.Types

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Heroku.Auth.Types as Heroku
import           Fission.Web.API.Auth.UCAN.Types

type Auth = "auth" :> UCAN

-- | Authorization check for the Heroku Addon API
type HerokuAddOnAPI = BasicAuth "heroku add-on api" Heroku.Auth

-- | Authorization check to return encoded DID for registering new users
type RegisterDID = AuthProtect "register-did"
type instance AuthClientData (AuthProtect "register-did") = Token

type HigherOrder = AuthProtect "higher-order"
type instance AuthClientData (AuthProtect "higher-order") = Token
