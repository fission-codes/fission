module Fission.Authorization.Access.Unchecked.Types (Unchecked (..)) where

import           Fission.Prelude

import           Fission.Models

data Unchecked privilege
  = AsUser (Entity User)              -- ^ All access for this user (think `su`)
  | privilege `DelegatedBy` Entity User -- ^ An *unchecked* privilege granted by a user
  deriving (Show, Eq)
