module Fission.CLI.Parser.Config.IPFS
  ( parser
  , binPathParser
  , timeoutParser
  -- * Reexports
  , module Fission.CLI.Parser.Config.IPFS.Types
  ) where

import           Options.Applicative

import qualified Network.IPFS.BinPath.Types           as IPFS
import qualified Network.IPFS.Timeout.Types           as IPFS

import           Fission.Prelude

import qualified Fission.CLI.Parser.Config.IPFS.Types as IPFS

-- Reexports

import           Fission.CLI.Parser.Config.IPFS.Types

parser :: Parser IPFS.Config
parser = do
  binPath        <- binPathParser
  timeoutSeconds <- timeoutParser
  pure IPFS.Config {..}

binPathParser :: Parser (Maybe IPFS.BinPath)
binPathParser =
  fmap fromPath . strOption $ mconcat
    [ help "Path to IPFS binary"
    , showDefaultWith \_ -> "`which ipfs`"
    ----------
    , long    "ipfs-bin"
    , metavar "BIN_PATH"
    ----------
    , value ""
    ]
  where
    fromPath ""   = Nothing
    fromPath path = Just $ IPFS.BinPath path

timeoutParser :: Parser IPFS.Timeout
timeoutParser =
  fmap IPFS.Timeout . option auto $ mconcat
    [ help "IPFS timeout"
    , showDefault
    ----------
    , long "ipfs-timeout"
    , metavar "SECONDS"
    ----------
    , value 300
    ]
