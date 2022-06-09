module Fission.CLI.Parser.Command.App.Delegate
  ( parserWithInfo
  , parser
  -- * Reexports
  , module Fission.CLI.Parser.Command.App.Delegate.Types
  ) where

import           Options.Applicative

import           Fission.Prelude

import           Fission.CLI.Parser.Internal

import           Fission.CLI.Parser.Command.App.Delegate.Types

import           Web.DID.Types as DID

parserWithInfo :: ParserInfo Options
parserWithInfo =
  parser `info` mconcat
    [ fullDesc
    , progDesc "Delegate append capability to a DID or key pair"
    ]

parser :: Parser Options
parser = do
  appName <- strOption $ mconcat
    [ help    "The app to append" 
    -----------
    , long    "app-name"
    , short   'a'
    -----------
    , metavar "NAME"
    ]

  generateKey  <- option boolean $ mconcat
    [ help  "Generate a key pair"
    , showDefault
    ----------
    , long  "generate-key"
    ----------
    , value True
    ]

  audienceDid <- option did $ mconcat
    [ help    "The DID audience"
    -----------
    , long    "did"
    , short   'd'
    -----------
    , metavar "DID"
    -----------
    , value Nothing
    ]

  return Options {..}

did :: ReadM (Maybe DID)
did = decodeStrict' <$> str