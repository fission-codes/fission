module Fission.Storage.IPFS (add) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy
import           RIO.Process (HasProcessContext)

import Data.Has

import           Fission.Internal.Constraint
import           Fission.IPFS.Types          as IPFS
import qualified Fission.IPFS.Process        as IPFS.Proc

add :: MonadRIO          cfg m
    => HasProcessContext cfg
    => HasLogFunc        cfg
    => Has IPFS.Path     cfg
    => Lazy.ByteString
    -> m IPFS.Address
add input = mkAddress <$> IPFS.Proc.run ["add", "-q"] input
