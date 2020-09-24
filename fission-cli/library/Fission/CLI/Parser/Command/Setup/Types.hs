module Fission.CLI.Parser.Command.Setup.Types (Options (..)) where

import           Fission.Prelude

import qualified Fission.CLI.Environment.OS.Types as OS
import           Fission.CLI.Parser.Verbose.Types

data Options = Options
  { verboseFlag :: !VerboseFlag
  , forceOS     :: !(Maybe OS.Supported)
  } deriving (Show, Eq)

instance Has VerboseFlag Options where
  hasLens = lens verboseFlag \opts newFlag ->
    opts {verboseFlag = newFlag }
