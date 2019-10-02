module Fission.Web.IPFS.Peer where

import RIO
import RIO.Process (HasProcessContext)

import Data.Has
import Database.Selda

import qualified Network.HTTP.Client as HTTP
import           Servant

import Fission.IPFS.Peer.Types
import Fission.Web.Server

type API = Get '[JSON, PlainText, OctetStream] [Peer]

all :: HasLogFunc        cfg
    => HasProcessContext cfg
    => RIOServer cfg API
all = do
  rawPeers <- IPFSProc.run' ["id"] -- ipfs id
  return $ filter rawPeers func
