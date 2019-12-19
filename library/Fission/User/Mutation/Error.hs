module Fission.User.Mutation.Error (Create (..)) where

import Servant.Server

import Fission.Prelude
import Fission.Web.Error
import qualified Fission.Internal.UTF8 as UTF8

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
  toServerError FailedDigest = err500 { errBody = UTF8.showLazyBS FailedDigest }
  toServerError AlreadyExists = err409 { errBody = UTF8.showLazyBS AlreadyExists }
