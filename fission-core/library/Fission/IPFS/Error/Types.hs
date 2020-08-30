module Fission.IPFS.Error.Types (UnableToConnect (..)) where

import           Fission.Prelude

data UnableToConnect = UnableToConnect
  deriving (Show, Eq, Exception)

instance Display UnableToConnect where
  display _ = "Unable to connect"
