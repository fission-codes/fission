module Fission.CLI.IPFS.Release.Types (Release (..)) where

import           Servant.API

import           Fission.CLI.Environment.OS.Types as OS
import qualified Fission.CLI.IPFS.Version.Types   as IPFS

import           Fission.Prelude

data Release = Release
  { version :: IPFS.Version
  , os      :: OS.Supported
  -- , arch :: Architecture
  }
  deriving (Show, Eq)

instance Display Release where
  display Release {..} =
    mconcat
      [ "kubo_"
      , display version
      , "_"
      , osLabel
      , "-amd64.tar.gz"
      ]
    where
      osLabel :: Utf8Builder
      osLabel =
        case os of
          Linux -> "linux"
          MacOS -> "darwin"

instance ToHttpApiData Release where
  toUrlPiece = textDisplay
