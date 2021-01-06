module Fission.Web.Server.Error.ActionNotAuthorized.Types (ActionNotAuthorized (..)) where

import           Fission.Prelude

import           Fission.Web.Server.Models

data ActionNotAuthorized entity
  = ActionNotAuthorized UserId
  deriving ( Show
           , Eq
           , Exception
           )

instance Display (ActionNotAuthorized entity) where
  display (ActionNotAuthorized userID) = "Action not authorized for user " <> display userID
