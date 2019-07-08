module Fission.Types (Fission) where

import RIO

import Fission.Config.Types

-- | Top-level application type
type Fission = RIO Config
