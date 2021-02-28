module Fission.CLI.Parser.Command.User.Login.Types (Options (..)) where

import           Fission.Prelude

-- import           Fission.User.Username.Types

import           Fission.CLI.Parser.Verbose.Types

data Options = Options
  { verboseFlag :: VerboseFlag -- ^ Verbose flag
  -- , username    :: Username -- FIXME
  } deriving (Show, Eq)

-- FIXME use composition instead of ad hoc subtyping
instance Has VerboseFlag Options where
  hasLens = lens verboseFlag \opts newFlag ->
    opts {verboseFlag = newFlag }
