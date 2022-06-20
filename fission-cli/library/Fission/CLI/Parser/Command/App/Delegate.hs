module Fission.CLI.Parser.Command.App.Delegate
  ( parserWithInfo
  , parser
  -- * Reexports
  , module Fission.CLI.Parser.Command.App.Delegate.Types
  ) where

import           Options.Applicative

import           Fission.Prelude

import           Fission.CLI.Parser.Command.App.Delegate.Types

import           Fission.Internal.UTF8

import           Web.DID.Types as DID

parserWithInfo :: ParserInfo Options
parserWithInfo =
  parser `info` mconcat
    [ fullDesc
    , progDesc "Delegate append capability to a key pair or DID. Generates a key pair by default."
    ]

parser :: Parser Options
parser = do
  appName <- strOption $ mconcat
    [ help    "The target app"
    -----------
    , long    "app-name"
    , short   'a'
    -----------
    , metavar "NAME"
    ]

  audienceDid <- option did $ mconcat
    [ help    "An audience DID"
    -----------
    , long    "did"
    , short   'd'
    -----------
    , metavar "DID"
    ]

  lifetimeInSeconds <- option auto $ mconcat
    [ help    "Lifetime in seconds before UCAN expires"
    , showDefault
    -----------
    , long    "lifetime"
    , short   'l'
    -----------
    , metavar "LIFETIME"
    -----------
    , value (300 :: Int)
    ]

  return Options {..}

did :: ReadM (Either String DID)
did = eitherDecodeStrict' . wrapIn "\"" <$> str