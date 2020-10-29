module Fission.Error.UserNotAuthorized.Types (UserNotAuthorized (..)) where

import           Servant

import           Fission.Models
import           Fission.Prelude
import           Fission.Web.Error.Class

data UserNotAuthorized entity
  = UserNotAuthorized UserId
  deriving ( Show
           , Eq
           , Exception
           )

instance Display (UserNotAuthorized entity) where
  display (UserNotAuthorized userId) = "Action not authorized for user " <> display userId

instance ToServerError (UserNotAuthorized entity) where
  toServerError _ = err401
