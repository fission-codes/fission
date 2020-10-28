module Fission.Authorization
  ( latestVersion
  -- * Reexports
  , module Fission.Authorization.Session.Types
  -- , module Fission.Authorization.Potency.Types
  , module Fission.Authorization.ServerDID
  ) where

-- import           Fission.Authorization.Potency.Types
import           Fission.Authorization.ServerDID
import           Fission.Authorization.Session.Types

import           Fission.SemVer.Types

latestVersion :: SemVer
latestVersion = SemVer 0 5 0
