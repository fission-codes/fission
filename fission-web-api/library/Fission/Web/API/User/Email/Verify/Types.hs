module Fission.Web.API.User.Email.Verify.Types (Verify) where

import           Fission.Web.API.Prelude

import           Fission.Challenge.Types

type Verify = "verify" :> Check

type Check
  =  Summary "Email verification"
  --
  :> Capture "Challenge" Challenge
  :> GetNoContent
