module Fission.Web.API.User.Verify.Types (Verify) where

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types as Auth

type Verify = "verify" :> Check

type Check
  =  Auth.HigherOrder
  --
  :> Summary "Validate auth"
  --
  :> Description "DEPRECATED ⛔ Verify user auth -- prefer /user/whoami"
  :> Get '[JSON] Bool
