module Fission.Web.API.User.Create.Types
  ( RoutesV_ (..)
  , WithDID
  , WithPassword
  ) where

import qualified Fission.User.Registration.Types as User

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types      as Auth

data RoutesV_ mode = RoutesV_
  { withDID      :: mode :- WithDID
  , withPassword :: mode :- WithPassword
  }
  deriving Generic

type WithDID
  =  Summary "Create user with DID and UCAN proof"
  :> Description "Register a new user (must auth with user-controlled DID)"
  --
  :> ReqBody '[JSON] User.Registration
  --
  :> Auth.RegisterDID
  :> PutCreated '[JSON] NoContent

type WithPassword
  =  Summary "Create user with password"
  :> Description "DEPRECATED ⛔ Register a new user (must auth with user-controlled DID)"
  --
  :> ReqBody '[JSON] User.Registration
  :> PostCreated '[JSON] ()
