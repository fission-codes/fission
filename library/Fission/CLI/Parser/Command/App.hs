module Fission.CLI.Parser.Command.App
  ( parserWithInfo
  , commands
  , queries
  , fallback
  -- * Reexports
  , module Fission.CLI.Parser.Command.App.Types
  ) where

import           Options.Applicative

import           Fission.Prelude

import qualified Fission.CLI.Parser.Command.App.Info  as Info
import qualified Fission.CLI.Parser.Command.App.Init  as Init
import qualified Fission.CLI.Parser.Command.App.Types as App
import qualified Fission.CLI.Parser.Command.App.Up    as Up

-- Reexports

import           Fission.CLI.Parser.Command.App.Types

parserWithInfo :: ParserInfo App.Options
parserWithInfo =
  (queries <|> commands <|> fallback) `info` mconcat
    [ fullDesc
    , header $ mconcat
        [ "This group of commands helps you create, publish, and maintain web apps. "
        , "If no subcommand is provided, the app info will be shown (if any)."
        ]
    , progDesc "User application management"
    ]

commands :: Parser App.Options
commands =
  hsubparser $ mconcat
    [ commandGroup "Commands"
    , metavar "COMMAND"
    , command "register" . fmap App.Init $ Init.parserWithInfo
    , command "publish"  . fmap App.Up   $ Up.parserWithInfo
    ]

queries :: Parser App.Options
queries =
  hsubparser $ mconcat
    [ commandGroup "Queries"
    , metavar "QUERY"
    , command "info" . fmap App.Info $ Info.parserWithInfo
    ]

fallback :: Parser App.Options
fallback = App.Info <$> Info.parser
