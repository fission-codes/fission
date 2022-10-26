module Fission.CLI.Parser.Command.UCAN
  ( parserWithInfo
  , commands
  -- * Reexports
  , module Fission.CLI.Parser.Command.App.Types
  ) where

import           Options.Applicative

import           Fission.Prelude

import qualified Fission.CLI.Parser.Command.UCAN.Generate as Generate

-- Reexports

import           Fission.CLI.Parser.Command.App.Types

parserWithInfo :: ParserInfo Generate.Options
parserWithInfo =
  commands `info` mconcat
    [ fullDesc
    , header $ mconcat
        [ "This group of commands helps you create, publish, and maintain web apps. "
        , "If no subcommand is provided, the app info will be shown (if any)."
        ]
    , progDesc "User application management"
    ]

commands :: Parser Generate.Options
commands =
  hsubparser $ mconcat
    [ commandGroup "Commands"
    , metavar "COMMAND"
    , command "ucan" $ Generate.parserWithInfo
    ]
