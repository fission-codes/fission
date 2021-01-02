module Fission.Error.ActionNotAuthorized.Types (ActionNotAuthorized (..)) where

import           Servant

import           Fission.Prelude
import           Fission.User.DID.Types

data ActionNotAuthorized entity
  = ActionNotAuthorized DID
  deriving ( Show
           , Eq
           , Exception
           )

instance Display (ActionNotAuthorized entity) where
  display (ActionNotAuthorized did) = "Action not authorized for user " <> display did
