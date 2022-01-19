module Fission.Authorization
  ( latestVersion
  -- * Reexports
  , module Web.Ucan.Potency.Types
  , module Fission.Authorization.ServerDID
  ) where

import           Fission.Authorization.ServerDID

import           Web.SemVer.Types
import           Web.Ucan.Potency.Types

latestVersion :: SemVer
latestVersion = SemVer 1 0 0
