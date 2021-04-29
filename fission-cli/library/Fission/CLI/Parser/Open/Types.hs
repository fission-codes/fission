module Fission.CLI.Parser.Open.Types (OpenFlag (..)) where

import           Fission.Prelude

newtype OpenFlag = OpenFlag
  { unFlag :: Bool }
  deriving newtype (Show, Eq)
