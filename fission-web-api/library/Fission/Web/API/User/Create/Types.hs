module Fission.Web.API.User.Create.Types
  ( Create
  , CreateWithDID
  , CreateWithPassword
  ) where

import qualified Fission.User.Registration.Types as User

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types      as Auth

type Create = CreateWithDID :<|> CreateWithPassword

type CreateWithDID
  =  Auth.RegisterDID
  --
  :> Summary "Create user with DID and UCAN proof"
  :> Description "Register a new user (must auth with user-controlled DID)"
  --
  :> ReqBody    '[JSON] User.Registration
  :> PutCreated '[JSON] NoContent

type CreateWithPassword
  =  Summary "Create user with password"
  :> Description "DEPRECATED ⛔ Register a new user (must auth with user-controlled DID)"
  --
  :> ReqBody     '[JSON] User.Registration
  :> PostCreated '[JSON] ()
