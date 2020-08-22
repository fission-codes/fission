module Fission.CLI.Parser.Command (parser) where

import           Options.Applicative

import           Fission.Prelude

import qualified Fission.CLI.Parser.Config.Remote         as Remote
import           Fission.CLI.Parser.Types

import qualified Fission.CLI.Parser.Command.App           as App
import qualified Fission.CLI.Parser.Command.User          as User

import           Fission.CLI.Parser.Command.Types         as Command
import qualified Fission.CLI.Parser.Command.Version       as Version

import qualified Fission.CLI.Parser.Command.App.Up        as App.Up
import qualified Fission.CLI.Parser.Command.User.Register as User.Register

parser :: Parser Options
parser = do
  cmd                      <- shortcuts <|> subCommands <|> version
  Remote.RemoteConfig {..} <- Remote.parser

  pure Options { fissionDID = mayDID
               , fissionURL = target
               , cmd
               }

version :: Parser Command
version = Version <$> Version.parser

shortcuts :: Parser Command
shortcuts =
  hsubparser $ mconcat
    [ commandGroup "Shortcuts"
    , metavar "SHORTCUT"
    , command "setup" $ Command.User . User.Register <$> User.Register.parserWithInfo
    , command "up"    $ Command.App  . App.Up <$> App.Up.parserWithInfo
    ]

subCommands :: Parser Command
subCommands =
  hsubparser $ mconcat
    [ commandGroup "Command Groups"
    , metavar "COMMAND"
    , command "app"  $ fmap Command.App  App.parserWithInfo
    , command "user" $ fmap Command.User User.parserWithInfo
    ]
