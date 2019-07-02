module Fission.Web.IPFS.Peer
  ( API
  , index
  ) where

import RIO
import RIO.Process (HasProcessContext)

import Servant
import Data.Has

import Fission.Web.Server
import Fission.Web.Error

import qualified Fission.IPFS.Types as IPFS
import qualified Fission.IPFS.Peer  as Peer

type API = Get '[JSON] [IPFS.Peer]

index :: Has IPFS.BinPath  cfg
      => HasProcessContext cfg
      => HasLogFunc        cfg
      => RIOServer         cfg API
index = Peer.all >>= ensureUnicode
