module Fission.Authorization
  ( latestVersion
  -- * Reexports
  , module Web.UCAN.Potency.Types
  , module Fission.Authorization.ServerDID
  ) where

import           Fission.Authorization.ServerDID

import           Web.SemVer.Types
import           Web.UCAN.Potency.Types

latestVersion :: SemVer
latestVersion = SemVer 1 0 0
