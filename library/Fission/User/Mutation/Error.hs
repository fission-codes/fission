module Fission.User.Mutation.Error (Create (..)) where

import Servant.Server

import Fission.Prelude
import Fission.Web.Error

data Create = FailedDigest
            | AlreadyExists

  deriving ( Exception
           , Eq
           , Generic
           , Show
           , ToJSON
           )

instance Display Create where
  display FailedDigest = "Could not create password digest"
  display AlreadyExists = "The username or email already exists in our system"

instance ToServerError Create where
  toServerError FailedDigest = err500 { errBody = "Could not create password digest" }
  toServerError AlreadyExists = err409 { errBody = "The username or email already exists in our system" }
