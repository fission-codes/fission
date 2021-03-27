module Fission.CLI.Parser.Command.Version.Types (Options (..)) where

import           Fission.Prelude

import           Fission.CLI.Parser.Verbose.Types

data Options = Options
  { verboseFlag :: VerboseFlag
  } deriving (Show, Eq)
