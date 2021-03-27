module Fission.CLI.Parser.Command.User.Register.Types (Options (..))  where

import           Fission.Prelude

import           Fission.User.Email.Types
import           Fission.User.Username.Types

data Options = Options
  { maybeUsername :: Maybe Username
  , maybeEmail    :: Maybe Email
  } deriving (Show, Eq)
