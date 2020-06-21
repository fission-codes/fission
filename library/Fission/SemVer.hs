module Fission.SemVer
  ( current
  -- * Reexports
  , module Fission.SemVer.Types
  ) where

import Fission.SemVer.Types

current :: SemVer
current = SemVer 1 0 0
