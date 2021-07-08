module Fission.CLI.Parser.Command.Setup
  ( parserWithInfo
  , parser
  , module Fission.CLI.Parser.Command.Setup.Types
  ) where

import           Options.Applicative
import qualified RIO.Text                               as Text

import           Fission.Prelude

import           Fission.CLI.Environment.OS.Types       as OS

import           Fission.CLI.Parser.Command.Setup.Types
import           Fission.User.Email.Types
import           Fission.User.Username.Types

parserWithInfo :: ParserInfo Options
parserWithInfo =
  parser `info` mconcat
    [ fullDesc
    , progDesc "Initial Fission setup"
    ]

parser :: Parser Options
parser = do
  forceOS       <- osParser

  maybeUsername <- option username $ mconcat
    [ help "The username to register"
    --------
    , long "username"
    , short 'u'
    --------
    , value Nothing
    , metavar "USERNAME"
    ]

  maybeEmail <- option email $ mconcat
    [ help "The email address for the account"
    --------
    , long "email"
    , short 'e'
    -------
    , value Nothing
    , metavar "EMAIL"
    ]

  maybeKeyFile <- option keyfile $ mconcat
    [ help "A root keyfile to import"
    -------
    , long "with-key"
    , short 'k'
    -------
    , value Nothing
    , metavar "KEYFILE"
    ]

  pure Options {..}

username :: ReadM (Maybe Username)
username = do
  raw <- str
  pure case raw of
    ""   -> Nothing
    name -> Just name

email :: ReadM (Maybe Email)
email = do
  raw <- str
  pure case raw of
    ""   -> Nothing
    mail -> Just mail

keyfile :: ReadM (Maybe FilePath)
keyfile = do
  raw <- str
  pure case raw of
    ""   -> Nothing
    path -> Just path


osParser :: Parser (Maybe OS.Supported)
osParser = do
  option mayOS $ mconcat
    [ help "Override OS detection"
    ----------
    , long    "os"
    , metavar "OS"
    ----------
    , value Nothing
    ]

mayOS :: ReadM (Maybe OS.Supported)
mayOS = do
  raw <- str
  case Text.toLower raw of
    "mac"   -> pure $ Just MacOS
    "macos" -> pure $ Just MacOS
    "linux" -> pure $ Just Linux
    ""      -> pure Nothing
    _       -> fail "Invalid OS option"
