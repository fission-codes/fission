module Fission.Ownership
  ( module Fission.Ownership.Class
  , isOwnedBy
  ) where

import           Prelude
import           Fission.Models

import           Fission.Ownership.Class

isOwnedBy :: Owned item => UserId -> item -> Bool
isOwnedBy userId item = userId == ownerId item
