module Fission.CLI.Parser.Command.User.WhoAmI
  ( parserWithInfo
  , parser
  -- * Reexports
  , module Fission.CLI.Parser.Command.User.WhoAmI.Types
  ) where

import           Options.Applicative

import           Fission.Prelude

import           Fission.CLI.Parser.Command.User.WhoAmI.Types

parserWithInfo :: ParserInfo Options
parserWithInfo =
  parser `info` mconcat
    [ fullDesc
    , progDesc "Display current user"
    ]

parser :: Parser Options
parser = pure CommandOnly
