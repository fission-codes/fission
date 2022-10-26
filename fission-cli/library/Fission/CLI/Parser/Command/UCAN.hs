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
    , header "This group of commands helps create and manage UCANs."
    , progDesc "UCAN management"
    ]

commands :: Parser Generate.Options
commands =
  hsubparser $ mconcat
    [ commandGroup "Commands"
    , metavar "COMMAND"
    , command "generate" $ Generate.parserWithInfo
    ]
