module Fission.Error.ActionNotAuthorized.Types (ActionNotAuthorized (..)) where

import           Servant

import           Fission.Models
import           Fission.Prelude
import           Fission.Web.Error.Class

data ActionNotAuthorized resource
  = ActionNotAuthorized
  deriving ( Show
           , Eq
           , Exception
           )

instance Display (ActionNotAuthorized entity) where
  display ActionNotAuthorized = "Action not authorized -- i.e. no valid proof available"

instance ToServerError (ActionNotAuthorized entity) where
  toServerError _ = err401
