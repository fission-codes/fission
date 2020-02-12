module Fission.User.Role.Types (Role (..)) where

import Database.Persist.TH

import Fission.Prelude

data Role
  = Regular
  | Admin
  deriving ( Show
           , Read
           , Eq
           )

derivePersistField "Role"
