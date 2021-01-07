module Fission.Web.Server.Auth.Types () where

import           Servant.API
import           Servant.Server.Experimental.Auth

import           Fission.User.DID.Types

type instance AuthServerData (AuthProtect "register-did") = DID
