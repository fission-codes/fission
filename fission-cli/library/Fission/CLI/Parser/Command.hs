module Fission.CLI.Parser.Command (parser) where

import           Options.Applicative

import           Fission.Prelude

import qualified Fission.CLI.Parser.Config.Remote       as Remote
import           Fission.CLI.Parser.Types
import qualified Fission.CLI.Parser.Verbose             as Verbose

import qualified Fission.CLI.Parser.Command.App         as App
import qualified Fission.CLI.Parser.Command.App.Up      as App.Up

import qualified Fission.CLI.Parser.Command.Generate    as Generate

import qualified Fission.CLI.Parser.Command.User        as User
import qualified Fission.CLI.Parser.Command.User.Login  as User.Login
import qualified Fission.CLI.Parser.Command.User.WhoAmI as User.WhoAmI

import qualified Fission.CLI.Parser.Command.Setup       as Setup
import           Fission.CLI.Parser.Command.Types       as Command
import qualified Fission.CLI.Parser.Command.Version     as Version

parser :: Parser Options
parser = do
  cmd         <- shortcuts <|> subCommands <|> version
  cfg         <- Remote.parser
  verboseFlag <- Verbose.parser

  -- NOTE GHC 8.10.3 doesn't like the wildcard. Use wildcards again later.
  pure Options { fissionDID = Remote.mayDID cfg
               , remote     = Remote.remote cfg
               , verboseFlag
               , cmd
               }

version :: Parser Command
version = Version <$> Version.parser

shortcuts :: Parser Command
shortcuts =
  hsubparser $ mconcat
    [ commandGroup "Shortcuts"
    , metavar "SHORTCUT"
    , command "setup"  $ Command.Setup              <$> Setup.parserWithInfo
    , command "up"     $ Command.App  . App.Up      <$> App.Up.parserWithInfo
    , command "whoami" $ Command.User . User.WhoAmI <$> User.WhoAmI.parserWithInfo
    , command "login"  $ Command.User . User.Login  <$> User.Login.parserWithInfo
 ]

subCommands :: Parser Command
subCommands =
  hsubparser $ mconcat
    [ commandGroup "Command Groups"
    , metavar "COMMAND"
    , command "app"      $ fmap Command.App      App.parserWithInfo
    , command "user"     $ fmap Command.User     User.parserWithInfo
    , command "generate" $ fmap Command.Generate Generate.parserWithInfo
    ]

