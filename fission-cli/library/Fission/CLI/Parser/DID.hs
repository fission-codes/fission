module Fission.CLI.Parser.DID
  ( parser
  , did
  ) where

import           Options.Applicative

import           Fission.Prelude

import           Web.DID.Types

parser :: Parser (Maybe DID)
parser =
  option did $ mconcat
    [ internal
    , help "Override the expected remote server DID"
    ----------
    , long    "remote-did"
    , metavar "REMOTE_DID"
    ----------
    , value Nothing
    ]

did :: ReadM (Maybe DID)
did = decodeStrict' <$> str
