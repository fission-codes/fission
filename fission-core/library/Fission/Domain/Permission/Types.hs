module Fission.Domain.Permission.Types (Permission (..)) where

import           Fission.Prelude

import           Fission.Models

import           Fission.Domain.Privilege.Capability.Types

data Permission = Permission
  { domain     :: !Domain -- NOTE no ID; primary key = domainDomainName
  , capability :: !Capability
  }
  deriving (Eq, Show)
