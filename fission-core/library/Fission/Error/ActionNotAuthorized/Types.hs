module Fission.Error.ActionNotAuthorized.Types (ActionNotAuthorized (..)) where

import           Servant

import           Fission.Prelude
import           Fission.Web.Error.Class
import           Fission.Models

data ActionNotAuthorized entity
  = ActionNotAuthorized UserId
  deriving ( Show
           , Eq
           , Exception
           )

instance Display (ActionNotAuthorized entity) where
  display (ActionNotAuthorized userId) = "Action not authorized for user " <> display userId

instance ToServerError (ActionNotAuthorized entity) where
  toServerError _ = err401
