module Fission.Web.API.User.Email.Resend.Types (Resend) where

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types as Auth

type Resend = "resend" :> Check

type Check
  =  Summary "Resend verification email"
  :> Description "Send a verification email to currently authenticated user."
  --
  :> Auth.HigherOrder
  :> PostNoContent
