module Fission.Authorization
  ( latestVersion
  -- * Reexports
  , module Web.JWT.Potency.Types
  , module Fission.Authorization.ServerDID
  ) where

import           Fission.Authorization.ServerDID

import           Web.JWT.Potency.Types
import           Web.SemVer.Types

latestVersion :: SemVer
latestVersion = SemVer 1 0 0
