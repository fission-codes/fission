module Fission.Ownership
  ( module Fission.Ownership.Class
  , isOwnedBy
  ) where

import           Fission.Models
import           Prelude

import           Fission.Ownership.Class

isOwnedBy :: Owned item => UserId -> item -> Bool
isOwnedBy userId item = userId == ownerId item
