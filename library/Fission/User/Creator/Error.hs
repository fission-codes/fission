module Fission.User.Creator.Error (AlreadyExists (..)) where

import           Servant.Server

import           Fission.Prelude
import           Fission.Web.Error

data AlreadyExists = AlreadyExists
  deriving ( Show
           , Eq
           , Exception
           )

instance Display AlreadyExists where
  display AlreadyExists = "The username or email already exists in our system"

instance ToServerError AlreadyExists where
  toServerError AlreadyExists =
    err409 { errBody = displayLazyBS AlreadyExists }
