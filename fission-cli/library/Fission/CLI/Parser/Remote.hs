module Fission.CLI.Parser.Remote
  ( parser
  , remote
  ) where

import           Options.Applicative

import           Fission.Prelude

import           Fission.CLI.Remote  as Remote

parser :: Parser Remote
parser =
  option remote $ mconcat
    [ internal
    , help "Which remote server"
    ----------
    , long    "remote"
    , short   'R'
    , metavar "PROD|STAGING|DEV|MOCK|<url>"
    ----------
    , value Remote.Production
    ]

remote :: ReadM Remote
remote = do
  txt <- str
  case Remote.fromText txt of
    Nothing -> fail $ "Unable to parse BaseUrl"
    Just r  -> pure r
