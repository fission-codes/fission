module Fission.CLI.Parser.Command.User.Types (Options (..)) where

import           Fission.Prelude

import qualified Fission.CLI.Parser.Command.User.Register as Register
import qualified Fission.CLI.Parser.Command.User.WhoAmI   as WhoAmI
import           Fission.CLI.Parser.Verbose.Types

data Options
  = Register Register.Options -- ^ Setup/Register the user
  | WhoAmI   WhoAmI.Options   -- ^ Show current logged in user
  deriving (Show, Eq)

instance Has VerboseFlag Options where
  getter = \case
    Register reg -> getter reg
    WhoAmI   who -> getter who

  modifier run = \case
    Register reg -> Register $ modifier run reg
    WhoAmI   who -> WhoAmI   $ modifier run who
