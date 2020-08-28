module Fission.User.Password.Error (FailedDigest (..)) where

import           Servant.Server

import           Fission.Prelude
import           Fission.Web.Error

data FailedDigest = FailedDigest
  deriving ( Show
           , Eq
           , Exception
           )

instance Display FailedDigest where
  display FailedDigest = "Could not create password digest"

instance ToServerError FailedDigest where
  toServerError FailedDigest =
    err500 { errBody = displayLazyBS FailedDigest }
