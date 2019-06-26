module Fission.Storage.IPFS
  ( add
  , addFile
  ) where

import           RIO
import qualified RIO.ByteString.Lazy as Lazy
import           RIO.Process (HasProcessContext)

import Data.Has
import Data.ByteString.Lazy.Char8 as BS

import           Fission.Internal.Constraint
import           Fission.IPFS.Types          as IPFS
import qualified Fission.IPFS.Process        as IPFS.Proc

add :: MonadRIO          cfg m
    => HasProcessContext cfg
    => HasLogFunc        cfg
    => Has IPFS.Path     cfg
    => Lazy.ByteString
    -> m IPFS.Address
add raw = mkAddress <$> IPFS.Proc.run ["add", "-q"] raw

addFile :: MonadRIO cfg m
        => HasProcessContext cfg
        => HasLogFunc        cfg
        => Has IPFS.Path     cfg
        => Lazy.ByteString
        -> String
        -> m Dir
addFile raw name = do
  result <- fmap mkAddress . BS.lines <$> IPFS.Proc.run opts raw
  case result of
    [inner, outer] -> return $ Root outer (File inner)

  where
    opts = [ "add"
           , "-wq"
           , "--stdin-name"
           , name
           ]

data Dir
  = File IPFS.Address
  | Dir (Map FileName Dir)
  | Root IPFS.Address Dir

type FileName = String


-- addFile :: MonadRIO cfg m
--     => HasProcessContext cfg
--     => HasLogFunc        cfg
--     => Has IPFS.Path     cfg
--     => Lazy.ByteString
--     -> Text
--     -> m IPFS.Address
-- addFile raw filename = do
--   innerHash <- addRaw raw
--   return [(filename, innerHash)]
