module Fission.Web.Client.User
  ( Register
  , Verify
  , WhoAmI
  , UpdatePK
  ) where

import           Servant
import qualified Fission.Web.User   as User
import           Fission.Web.Routes (UserPrefix)

type Register = UserPrefix :> User.RegisterRoute
type Verify   = UserPrefix :> User.VerifyRoute
type WhoAmI   = UserPrefix :> User.WhoAmIRoute
type UpdatePK = UserPrefix :> User.UpdatePublicKeyRoute
