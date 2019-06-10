module Fission.Storage.IPFS (add) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import Data.Has

import           Fission.Config
import           Fission.Internal.Constraint
import           Fission.IPFS.Address        as IPFS
import qualified Fission.IPFS.Process        as IPFS.Proc

add :: (MonadRIO cfg m, Has IPFSPath cfg) => Lazy.ByteString -> m IPFS.Address
add input = mkAddress <$> IPFS.Proc.run ["add", "-q"] input
