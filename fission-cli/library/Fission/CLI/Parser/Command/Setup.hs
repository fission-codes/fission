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
import qualified Fission.CLI.Parser.Verbose             as Verbose

parserWithInfo :: ParserInfo Options
parserWithInfo =
  parser `info` mconcat
    [ fullDesc
    , progDesc "Initial Fission setup"
    ]

parser :: Parser Options
parser = do
  verboseFlag <- Verbose.parser
  forceOS     <- osParser
  pure Options {..}

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
    "nix"   -> pure $ Just NixOS
    "nixos" -> pure $ Just NixOS
    "mac"   -> pure $ Just MacOS
    "macos" -> pure $ Just MacOS
    "linux" -> pure $ Just Linux
    ""      -> pure Nothing
    _       -> fail "Invalid OS option"
