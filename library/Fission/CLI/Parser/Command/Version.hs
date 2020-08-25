module Fission.CLI.Parser.Command.Version (parser) where

import           Options.Applicative

import           Fission.Prelude

import           Fission.CLI.Parser.Command.Version.Types
import           Fission.CLI.Parser.Verbose.Types

parser :: Parser Options
parser = do
  let verboseFlag = VerboseFlag False

  _ <- switch $ mconcat
    [ help  "Print version"
    ----------
    , long  "version"
    ]

  pure Options {..}
