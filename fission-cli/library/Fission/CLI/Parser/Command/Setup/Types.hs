module Fission.CLI.Parser.Command.Setup.Types (Options (..)) where

import           Fission.Prelude

import qualified Fission.CLI.Environment.OS.Types as OS
import           Fission.CLI.Parser.Verbose.Types
import           Fission.User.Email.Types
import           Fission.User.Username.Types

data Options = Options
  { verboseFlag   :: VerboseFlag
  , forceOS       :: Maybe OS.Supported
  , maybeUsername :: Maybe Username
  , maybeEmail    :: Maybe Email
  } deriving (Show, Eq)

instance Has VerboseFlag Options where
  hasLens = lens verboseFlag \opts newFlag ->
    opts {verboseFlag = newFlag }
