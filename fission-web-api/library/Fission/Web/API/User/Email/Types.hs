module Fission.Web.API.User.Email.Types (Routes (..), Verify) where

import           Fission.Challenge.Types

import           Fission.Web.API.Prelude

import           Fission.User.Username.Types
import qualified Fission.Web.API.Auth.Types  as Auth

data Routes mode = Routes
  { verify ::
      mode
      :- "verify"
      :> Verify

  , resend ::
      mode
      :- "resend"
      :> Summary "Resend verification email"
      :> Description "Send a verification email to currently authenticated user."
      --
      :> Auth.HigherOrder
      :> PostNoContent

  , recover ::
      mode
      :- "recover"
      :> Summary "Email verification for account recovery"
      --
      :> Capture "Username" Username
      --
      :> PostNoContent
  }
  deriving Generic


type Verify
  =  Summary "Email verification"
  --
  :> Capture "Challenge" Challenge
  --
  :> GetNoContent
