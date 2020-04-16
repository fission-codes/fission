module Fission.SemVer
  ( current
  -- * Reexports
  , module Fission.SemVer.Types
  ) where

import Fission.SemVer.Types

current :: SemVer
current = SemVer 0 1 0
