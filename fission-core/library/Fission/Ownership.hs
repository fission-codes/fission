module Fission.Ownership
  ( module Fission.Ownership.Class
  , isOwnedBy
  ) where

import           Fission.Models
import           Prelude

import           Fission.Ownership.Class

isOwnedBy :: Owned item => item -> UserId -> Bool
item `isOwnedBy` userId = userId == ownerId item
