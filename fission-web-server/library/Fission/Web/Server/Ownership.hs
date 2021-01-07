module Fission.Web.Server.Ownership
  ( module Fission.Web.Server.Ownership.Class
  , isOwnedBy
  ) where

import           Prelude

import           Fission.Web.Server.Models
import           Fission.Web.Server.Ownership.Class

isOwnedBy :: Owned item => UserId -> item -> Bool
isOwnedBy userId item = userId == ownerId item
