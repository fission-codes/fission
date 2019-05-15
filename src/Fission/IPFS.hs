{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Fission.IPFS
  ( Address
  , Peer (..)
  , mkAddress
  , upload
  , peers
  ) where

import Fission.IPFS.Address

import           RIO
import qualified RIO.ByteString.Lazy as Lazy

import Data.Has

import           Fission.IPFS.Address as IPFS
import qualified Fission.IPFS.Process as IPFS.Proc
import           Fission.IPFS.Peer

import Fission.Config
import Fission.Internal.Constraint

upload :: (MonadRIO cfg m, Has IpfsPath cfg) => Lazy.ByteString -> m IPFS.Address
upload input = mkAddress <$> IPFS.Proc.run ["add", "-q"] input
