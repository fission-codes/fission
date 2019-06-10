module Fission.Web.IPFS.Peer
  ( API
  , server
  ) where

import RIO

import Servant
import Data.Has

import Fission.Config
import Fission.Web.Server
import Fission.Web.Error

import qualified Fission.IPFS.Peer as IPFS
import qualified Fission.IPFS.Peer as Peer

type API = Get '[JSON] [IPFS.Peer]

server :: Has IPFSPath cfg => RIOServer cfg API
server = Peer.all >>= ensureUnicode
