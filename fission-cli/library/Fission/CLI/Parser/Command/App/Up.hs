module Fission.CLI.Parser.Command.App.Up
  ( parserWithInfo
  , parser
  -- * Reexports
  , module Fission.CLI.Parser.Command.App.Up.Types
  ) where

import           Options.Applicative

import           Fission.Prelude

import           Fission.CLI.Parser.Command.App.Up.Types
import qualified Fission.CLI.Parser.Config.IPFS          as IPFS
import           Fission.CLI.Parser.Internal
import qualified Fission.CLI.Parser.Verbose              as Verbose
import           Fission.CLI.Parser.Watch.Types

parserWithInfo :: ParserInfo Options
parserWithInfo =
  parser `info` mconcat
    [ fullDesc
    , progDesc "Upload the working directory"
    ]

parser :: Parser Options
parser = do
  ipfsCfg     <- IPFS.parser
  verboseFlag <- Verbose.parser

  updateData <- option boolean $ mconcat
    [ help  "Upload the data"
    , showDefault
    ----------
    , long  "update-data"
    ----------
    , value True
    ]

  updateDNS <- option boolean $ mconcat
    [ help  "Update DNS"
    , showDefault
    ----------
    , long  "update-dns"
    ----------
    , value True
    ]

  watch <- fmap WatchFlag . switch $ mconcat
    [ help  "Watch for changes & automatically trigger upload"
    ----------
    , long  "watch"
    , short 'w'
    ]

  filePath <- strArgument $ mconcat
    [ help    "The file path of the assets or directory to sync"
    , showDefault
    ----------
    , metavar "PATH"
    ----------
    , value   "./"
    ]

  return Options {..}
