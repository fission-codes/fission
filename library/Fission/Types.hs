-- | Top level types
module Fission.Types (Fission) where

import Fission.Config.Types
import Fission.Prelude

-- | Top-level application type
type Fission = RIO Config
