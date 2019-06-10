module Fission.Web.IPFS
  ( API
  , server
  ) where

import RIO

import Servant
import Data.Has

import           Fission.Config
import           Fission.Web.Server
import qualified Fission.Web.IPFS.Peer   as Peer
import qualified Fission.Web.IPFS.Upload as Upload

type API = {- Root -} Upload.API
      :<|> "peers" :> Peer.API

server :: (HasLogFunc cfg, Has IPFSPath cfg) => RIOServer cfg API
server = Upload.server :<|> Peer.server
