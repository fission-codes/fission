module Fission.CLI.Parser.Command.App.Init.Types (Options (..)) where

import           Fission.Prelude

import qualified Fission.App.Name                     as App

import qualified Fission.CLI.Parser.Config.IPFS.Types as IPFS

-- | Arguments, flags & switches for the `app init` command
data Options = Options
  { appDir     :: FilePath
  , buildDir   :: Maybe FilePath
  , mayAppName :: Maybe App.Name
  , ipfsCfg    :: IPFS.Config
  } deriving (Show, Eq)
