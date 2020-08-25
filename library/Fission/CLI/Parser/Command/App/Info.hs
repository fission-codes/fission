module Fission.CLI.Parser.Command.App.Info
  ( parserWithInfo
  , parser
  -- * Reexports
  , module Fission.CLI.Parser.Command.App.Info.Types
  ) where

import           Options.Applicative

import           Fission.Prelude

import           Fission.CLI.Parser.Command.App.Info.Types
import qualified Fission.CLI.Parser.Verbose                as Verbose

parserWithInfo :: ParserInfo Options
parserWithInfo =
  parser `info` mconcat
    [ fullDesc
    , progDesc "Detail about the current app"
    ]

parser :: Parser Options
parser = do
  verboseFlag <- Verbose.parser
  pure Options {..}
