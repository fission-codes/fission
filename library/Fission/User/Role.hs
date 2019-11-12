module Fission.User.Role (Role (..)) where

import Database.Selda (SqlType)

import Fission.Prelude

data Role
  = Regular
  | Admin
  deriving ( Show
           , Read
           , Eq
           , Enum
           , Bounded
           , SqlType
           )
