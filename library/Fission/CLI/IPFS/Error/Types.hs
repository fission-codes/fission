module Fission.CLI.IPFS.Error.Types (Err (..)) where

import           Fission.Prelude

data Err = UnableToConnect
  deriving (Show, Exception)
