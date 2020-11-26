module Fission.CLI.Parser.Command.User.Register.Types (Options (..))  where

import           Fission.Prelude

import           Fission.CLI.Parser.Verbose.Types
import           Fission.User.Username.Types
import           Fission.User.Email.Types

data Options = Options
  { maybeUsername :: !(Maybe Username)
  , maybeEmail    :: !(Maybe Email)
  , verboseFlag :: !VerboseFlag -- ^ Verbose flag
  } deriving (Show, Eq)

instance Has VerboseFlag Options where
  hasLens = lens verboseFlag \opts newFlag ->
    opts {verboseFlag = newFlag }
