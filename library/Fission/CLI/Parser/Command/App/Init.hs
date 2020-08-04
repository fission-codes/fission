module Fission.CLI.Parser.Command.App.Init
  ( parser
  , parserWithInfo
  -- * Reexport
  , module Fission.CLI.Parser.Command.App.Init.Types
  ) where

import           Options.Applicative

import           Fission.Prelude

import           Fission.CLI.Parser.Command.App.Init.Types
import qualified Fission.CLI.Parser.Config.IPFS            as IPFS
import qualified Fission.CLI.Parser.Verbose                as Verbose

parserWithInfo :: ParserInfo Options
parserWithInfo =
  parser `info` mconcat
    [ fullDesc
    , progDesc "Initialize an existing app"
    ]

parser :: Parser Options
parser = do
  ipfsCfg     <- IPFS.parser
  verboseFlag <- Verbose.parser

  appDir <- strOption $ mconcat
    [ help    "The file path to initialize the app in (app config, etc)"
    , showDefault
    -----------
    , long    "app-dir"
    , short   'a'
    -----------
    , value   "."
    , metavar "APP_PATH"
    ]

  buildDir <- strOption $ mconcat
    [ help    "The file path of the assets or directory to sync"
    -----------
    , value   ""
    , metavar "BUILD_PATH"
    -----------
    , long    "build-dir"
    , short   'b'
    ]

  return Options {..}
