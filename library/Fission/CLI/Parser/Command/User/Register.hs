module Fission.CLI.Parser.Command.User.Register
  ( parserWithInfo
  , parser
  -- * Reexport
  , module Fission.CLI.Parser.Command.User.Register.Types
  ) where

import           Options.Applicative

import           Fission.Prelude

import           Fission.CLI.Parser.Command.User.Register.Types
import qualified Fission.CLI.Parser.Verbose                     as Verbose

parserWithInfo :: ParserInfo Options
parserWithInfo =
  parser `info` mconcat
    [ fullDesc
    , progDesc "Set up system"
    ]

parser :: Parser Options
parser = do
  verboseFlag <- Verbose.parser
  pure Options {..}
