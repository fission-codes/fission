module Fission.CLI.Parser.Config.IPFS.Types (Config (..)) where

import qualified Network.IPFS.BinPath.Types as IPFS
import qualified Network.IPFS.Timeout.Types as IPFS

import           Fission.Prelude

data Config = Config
  { binPath        :: !(Maybe IPFS.BinPath) -- ^ Path to the IPFS binary (defaults to system)
  , timeoutSeconds :: !IPFS.Timeout         -- ^ IPFS timeout
  } deriving (Show, Eq)
