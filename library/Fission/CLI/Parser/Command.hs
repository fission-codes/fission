module Fission.CLI.Parser.Command (parser) where

import           Options.Applicative

import           Fission.Prelude

import qualified Fission.CLI.Parser.Config.Remote   as Remote
import           Fission.CLI.Parser.Types

import qualified Fission.CLI.Parser.Command.App     as App
import qualified Fission.CLI.Parser.Command.User    as User

import           Fission.CLI.Parser.Command.Types   as Command
import qualified Fission.CLI.Parser.Command.Version as Version

parser :: Parser Options
parser = do
  cmd                      <- sub <|> version
  Remote.RemoteConfig {..} <- Remote.parser

  pure Options { fissionDID = mayDID
               , fissionURL = target
               , cmd
               }
  where
    version :: Parser Command
    version = Version <$> Version.parser

    sub :: Parser Command
    sub =
      hsubparser $ mconcat
        [ command "app"  $ fmap Command.App  App.parserWithInfo
        , command "user" $ fmap Command.User User.parserWithInfo
        ]
