module Fission.CLI.Parser.Command.App.Init.Types (Options (..)) where

import           Fission.Prelude

import qualified Fission.CLI.Parser.Config.IPFS.Types as IPFS
import           Fission.CLI.Parser.Verbose.Types

-- | Arguments, flags & switches for the `app init` command
data Options = Options
  { appDir      :: !FilePath
  , buildDir    :: !FilePath
  , ipfsCfg     :: !IPFS.Config
  , verboseFlag :: !VerboseFlag -- ^ Verbose flag
  } deriving (Show, Eq)

instance Has VerboseFlag Options where
  hasLens = lens verboseFlag \opts newFlag ->
    opts {verboseFlag = newFlag }
