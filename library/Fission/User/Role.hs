module Fission.User.Role (Role (..)) where

import Fission.Prelude
import qualified Fission.Storage.Database as Database


data Role
  = Regular
  | Admin
  deriving ( Show
           , Read
           , Eq
           )

Database.generateInstances "Role"
