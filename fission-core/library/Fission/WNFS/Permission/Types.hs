module Fission.WNFS.Permission.Types (Permission (..)) where

import           Fission.Prelude

import           Fission.Models

import           Fission.WNFS.Privilege.Capability.Types
import           Fission.WNFS.Privilege.Types
import           Fission.WNFS.Subgraph.Types

data Permission = Permission
  { owner     :: !(Entity User)
  , privilege :: !Privilege
  }
  deriving (Show, Eq)
