module Fission.Authorization
  ( latestVersion
  -- * Reexports
  , module Fission.Web.Auth.Token.UCAN.Potency.Types
  , module Fission.Authorization.ServerDID
  ) where

import           Fission.Authorization.ServerDID

import           Fission.Web.Auth.Token.UCAN.Potency.Types
import           Web.SemVer.Types

latestVersion :: SemVer
latestVersion = SemVer 1 0 0
