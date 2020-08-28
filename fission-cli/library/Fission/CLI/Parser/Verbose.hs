module Fission.CLI.Parser.Verbose
  ( parser
  , module Fission.CLI.Parser.Verbose.Types
  ) where

import           Options.Applicative

import           Fission.Prelude

import           Fission.CLI.Parser.Verbose.Types

parser :: Parser VerboseFlag
parser =
  fmap VerboseFlag . switch $ mconcat
    [ help "Detailed output"
    ----------
    , long "verbose"
    , short 'v'
    ]
