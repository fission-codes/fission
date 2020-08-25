module Fission.CLI.Parser (parserWithInfo) where

import           Options.Applicative

import           Fission.Prelude

import           Fission.CLI.Parser.Command as Command
import qualified Fission.CLI.Parser.Types   as CLI

parserWithInfo :: ParserInfo CLI.Options
parserWithInfo =
  (Command.parser <**> helper) `info` mconcat
    [ fullDesc
    , progDesc "CLI to interact with Fission services"
    , header   "Fission makes developing, deploying, updating, and iterating on web apps quick and easy."
    , footer   "Visit https://fission.codes for more information and support"
    ]

