module Fission.CLI.Parser.Command.Setup.Types (Options (..)) where

import           Fission.Prelude

import qualified Fission.CLI.Environment.OS.Types as OS
import           Fission.User.Email.Types
import           Fission.User.Username.Types

data Options = Options
  { forceOS       :: Maybe OS.Supported
  , maybeUsername :: Maybe Username
  , maybeEmail    :: Maybe Email
  , maybeKeyFile  :: Maybe FilePath
  } deriving (Show, Eq)
