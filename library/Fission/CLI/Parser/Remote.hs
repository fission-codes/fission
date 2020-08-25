module Fission.CLI.Parser.Remote
  ( parser
  , remote
  ) where

import           Options.Applicative
import           Servant.Client

import           Fission.Prelude

import           Fission.CLI.Remote  as Remote

parser :: Parser BaseUrl
parser =
  option remote $ mconcat
    [ internal
    , help "Which remote server"
    ----------
    , long    "remote"
    , short   'R'
    , metavar "PROD|STAGING|DEV|MOCK|<url>"
    ----------
    , value Remote.production
    ]

remote :: ReadM BaseUrl
remote = do
  txt <- str
  case Remote.toBaseUrl =<< Remote.fromText txt of
    Nothing      -> fail $ "Unable to parse BaseUrl"
    Just baseUrl -> pure baseUrl
