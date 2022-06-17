module Fission.CLI.Parser.Command.Generate.Credentials
  ( parserWithInfo
  , parser
  -- * Reexports
  , module Fission.CLI.Parser.Command.Generate.Credentials.Types
  ) where

import           Options.Applicative

import           Fission.Prelude

import           Fission.CLI.Parser.Command.Generate.Credentials.Types


parserWithInfo :: ParserInfo Options
parserWithInfo =
  parser `info` mconcat
    [ fullDesc
    , progDesc "Generate a key pair and DID"
    ]

parser :: Parser Options
parser = pure CommandOnly