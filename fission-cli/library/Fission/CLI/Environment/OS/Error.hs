module Fission.CLI.Environment.OS.Error (Unsupported (..)) where

import           Fission.Prelude

newtype Unsupported = Unsupported { reportedOS :: String }
  deriving newtype  (Show, Eq)
  deriving anyclass Exception
