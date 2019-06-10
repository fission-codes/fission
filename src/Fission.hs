module Fission (Fission) where

import RIO

import Fission.Config

-- | Top-level application type
type Fission = RIO Config
