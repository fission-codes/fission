module Fission.CLI.Parser.Command.User.Login
  ( parserWithInfo
  , parser
  -- * Reexport
  , module Fission.CLI.Parser.Command.User.Login.Types
  ) where

import           Options.Applicative

import           Fission.Prelude

import           Fission.CLI.Parser.Command.User.Login.Types
import qualified Fission.CLI.Parser.Verbose                  as Verbose

parserWithInfo :: ParserInfo Options
parserWithInfo =
  parser `info` mconcat
    [ fullDesc
    , progDesc "Log in to an existing account"
    ]

parser :: Parser Options
parser = do
  verboseFlag <- Verbose.parser

  username <- option str $ mconcat
    [ help  "Username"
    ---
    , long  "username"
    , short 'u'
    ]

  pure Options {..}
