module Fission.Process.Time.Error (TimedOut (..)) where

import           Fission.Prelude

data TimedOut = TimedOut
  deriving (Show, Eq, Exception)
