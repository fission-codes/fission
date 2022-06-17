module Fission.CLI.Parser.Command.Generate
  ( parserWithInfo
  , parser
  -- * Reexports
  , module Fission.CLI.Parser.Command.Generate.Types
  ) where

import           Options.Applicative

import           Fission.Prelude

import           Fission.CLI.Parser.Command.Generate.Types
import qualified Fission.CLI.Parser.Command.Generate.Credentials as Credentials

parserWithInfo :: ParserInfo Options
parserWithInfo =
  parser `info` mconcat
    [ fullDesc
    , header   "Generate commands"
    , progDesc "Generate key pairs and DIDs"
    ]

parser :: Parser Options
parser =
  hsubparser $ mconcat
    [ command "credentials"    $ fmap Credentials Credentials.parserWithInfo
    ]