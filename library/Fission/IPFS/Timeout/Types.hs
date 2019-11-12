module Fission.IPFS.Timeout.Types (Timeout (..)) where

import System.Envy

import Fission.Prelude
import Fission.Internal.Orphanage.Natural ()

newtype Timeout = Timeout { getSeconds :: Natural }
  deriving          ( Eq
                    , Show
                    , Generic
                    )
  deriving newtype  ( Num )

instance FromEnv Timeout where
  fromEnv _ = Timeout <$> env "IPFS_TIMEOUT"

instance FromJSON Timeout where
  parseJSON = withScientific "IPFS.Timeout" \num ->
    Timeout <$> parseJSON (Number num)
