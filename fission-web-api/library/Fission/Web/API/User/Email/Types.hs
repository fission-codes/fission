module Fission.Web.API.User.Email.Types (Email) where

import           Fission.Web.API.Prelude

import           Fission.Web.API.User.Email.Verify.Types

type Email = "email" :> Verify
