module Fission.App.Permission.Types (Permission (..)) where

import           Fission.Prelude

import           Fission.Models

import           Fission.App.Privilege.Capability.Types

data Permission = Permission
  { app        :: !(Entity App)
  , appDomain  :: !(Entity AppDomain)
  , capability :: !Capability
  }
  deriving (Eq, Show)
