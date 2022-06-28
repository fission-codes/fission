module Fission.CLI.Parser.Quiet.Types (QuietFlag (..)) where

import           Fission.Prelude

newtype QuietFlag = QuietFlag
  { unFlag :: Bool }
  deriving newtype (Show, Eq)
