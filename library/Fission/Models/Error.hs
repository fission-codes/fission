module Fission.Models.Error
  ( NotFound            (..)
  , ActionNotAuthorized (..)
  ) where

import Servant

import Fission.Prelude
import Fission.Web.Error.Class
import Fission.Models

data NotFound entity
  = NotFound
  deriving ( Show
           , Eq
           , Exception
           )

instance ToServerError (NotFound entity) where
  toServerError _ = err404

data ActionNotAuthorized entity
  = ActionNotAuthorized UserId
  deriving ( Show
           , Eq
           , Exception
           )

instance ToServerError (ActionNotAuthorized entity) where
  toServerError _ = err401
