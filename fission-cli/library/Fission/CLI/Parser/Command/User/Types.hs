module Fission.CLI.Parser.Command.User.Types (Options (..)) where

import           Fission.Prelude

import qualified Fission.CLI.Parser.Command.User.Login    as Login
import qualified Fission.CLI.Parser.Command.User.Register as Register
import qualified Fission.CLI.Parser.Command.User.WhoAmI   as WhoAmI
import           Fission.CLI.Parser.Verbose.Types

data Options
  = Login    Login.Options    -- ^ Log in to existing account
  | Register Register.Options -- ^ Setup/Register the user
  | WhoAmI   WhoAmI.Options   -- ^ Show current logged in user
  deriving (Show, Eq)

instance Has VerboseFlag Options where
  getter = \case
    Login    lin -> getter lin
    Register reg -> getter reg
    WhoAmI   who -> getter who

  modifier run = \case
    Login    lin -> Login    $ modifier run lin
    Register reg -> Register $ modifier run reg
    WhoAmI   who -> WhoAmI   $ modifier run who
