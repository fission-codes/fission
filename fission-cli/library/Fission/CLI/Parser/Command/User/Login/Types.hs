module Fission.CLI.Parser.Command.User.Login.Types (Options (..)) where

import           Fission.Prelude

import           Fission.User.Username.Types

data Options = Options
  { optUsername :: Maybe Username
  } deriving (Show, Eq)
