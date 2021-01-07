module Fission.CLI.Parser.Command.User.Login.Types (Options (..)) where

import           Fission.Prelude

import           Fission.User.Username.Types

import           Fission.CLI.Parser.Verbose.Types

data Options = Options
  { verboseFlag :: VerboseFlag -- ^ Verbose flag
  , username    :: Username
  } deriving (Show, Eq)

instance Has VerboseFlag Options where
  hasLens = lens verboseFlag \opts newFlag ->
    opts {verboseFlag = newFlag }
