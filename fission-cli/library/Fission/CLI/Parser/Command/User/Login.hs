module Fission.CLI.Parser.Command.User.Login
  ( parserWithInfo
  , parser
  -- * Reexport
  , module Fission.CLI.Parser.Command.User.Login.Types
  ) where

import           Options.Applicative

import           Fission.Prelude

import           Fission.User.Username.Types

import           Fission.CLI.Parser.Command.User.Login.Types

parserWithInfo :: ParserInfo Options
parserWithInfo =
  parser `info` mconcat
    [ fullDesc
    , progDesc "Log in to an existing account"
    ]

parser :: Parser Options
parser = do
  optUsername <- option username $ mconcat
    [ help  "Username"
    ---
    , long  "username"
    , short 'u'
    ]

  pure Options {..}

username :: ReadM (Maybe Username)
username = do
  raw <- str
  pure case raw of
    ""   -> Nothing
    name -> Just name
