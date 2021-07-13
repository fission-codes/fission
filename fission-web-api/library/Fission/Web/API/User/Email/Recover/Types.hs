module Fission.Web.API.User.Email.Recover.Types (Recover) where

import           Fission.Web.API.Prelude

import           Fission.User.Username.Types

type Recover = "recover" :> Check

type Check
  =  Summary "Email verification for account recovery"
  --
  :> Capture "Username" Username
  --
  :> PostNoContent
