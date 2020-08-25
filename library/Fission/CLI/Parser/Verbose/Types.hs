module Fission.CLI.Parser.Verbose.Types (VerboseFlag (..)) where

import           Fission.Prelude

newtype VerboseFlag = VerboseFlag { unFlag :: Bool }
  deriving newtype (Show, Eq)

instance Display VerboseFlag where
  display (VerboseFlag flag) =
    if flag
      then "Verbose"
      else "NonVerbose"
