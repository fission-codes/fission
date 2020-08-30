module Fission.CLI.Parser.Command.App.Info.Types (Options (..))  where

import           Fission.Prelude

import           Fission.CLI.Parser.Verbose.Types

data Options = Options
  { verboseFlag :: !VerboseFlag -- ^ Verbose flag
  } deriving (Show, Eq)

instance Has VerboseFlag Options where
  hasLens = lens verboseFlag \opts newFlag ->
    opts {verboseFlag = newFlag }
