module Fission.Web.Client.User
  ( Register
  , Verify
  , WhoAmI
  , UpdatePK
  ) where

import           Servant.API

import           Fission.Web.Routes (UserPrefix)
import qualified Fission.Web.User   as User

type Register = UserPrefix :> User.RegisterRoute
type Verify   = UserPrefix :> User.VerifyRoute
type WhoAmI   = UserPrefix :> User.WhoAmIRoute
type UpdatePK = UserPrefix :> User.UpdatePublicKeyRoute
