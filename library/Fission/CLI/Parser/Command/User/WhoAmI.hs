module Fission.CLI.Parser.Command.User.WhoAmI
  ( parserWithInfo
  , parser
  -- * Reexports
  , module Fission.CLI.Parser.Command.User.WhoAmI.Types
  ) where

import           Options.Applicative

import           Fission.Prelude

import           Fission.CLI.Parser.Command.User.WhoAmI.Types
import qualified Fission.CLI.Parser.Verbose                   as Verbose

parserWithInfo :: ParserInfo Options
parserWithInfo =
  parser `info` mconcat
    [ fullDesc
    , progDesc "Display current user"
    ]

parser :: Parser Options
parser = do
  verboseFlag <- Verbose.parser
  pure Options {..}
