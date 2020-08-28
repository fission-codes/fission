module Fission.CLI.Parser.Command.Types (Command (..)) where

import qualified RIO.Text                                 as Text

import           Fission.Prelude

import           Fission.CLI.Parser.Verbose.Types

import qualified Fission.CLI.Parser.Command.App.Types     as App
import qualified Fission.CLI.Parser.Command.User.Types    as User
import qualified Fission.CLI.Parser.Command.Version.Types as Version

data Command
  = Version Version.Options
  | App     App.Options
  | User    User.Options
  deriving (Eq, Show)

instance Display Command where
  textDisplay = Text.pack . show

instance Has VerboseFlag Command where
  getter = \case
    Version options -> getter options
    App     options -> getter options
    User    options -> getter options

  modifier run = \case
    Version options -> Version $ modifier run options
    App     options -> App     $ modifier run options
    User    options -> User    $ modifier run options
