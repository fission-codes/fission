module Fission.User.Role (Role (..)) where

import Fission.Prelude
import qualified Fission.Storage.Persist as Persist


data Role
  = Regular
  | Admin
  deriving ( Show
           , Read
           , Eq
           )

Persist.generateInstances "Role"
