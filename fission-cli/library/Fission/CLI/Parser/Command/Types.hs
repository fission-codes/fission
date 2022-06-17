module Fission.CLI.Parser.Command.Types (Command (..)) where

import qualified RIO.Text                                 as Text

import           Fission.Prelude

import qualified Fission.CLI.Parser.Command.App.Types      as App
import qualified Fission.CLI.Parser.Command.Generate.Types as Generate
import qualified Fission.CLI.Parser.Command.Setup.Types    as Setup
import qualified Fission.CLI.Parser.Command.User.Types     as User
import qualified Fission.CLI.Parser.Command.Version.Types  as Version

data Command
  = Version  Version.Options
  | App      App.Options
  | User     User.Options
  | Setup    Setup.Options
  | Generate Generate.Options
  deriving (Eq, Show)

instance Display Command where
  textDisplay = Text.pack . show
