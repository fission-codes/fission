module Fission.Web.IPFS
  ( API
  , server
  ) where

import RIO
import RIO.Process (HasProcessContext)

import Servant
import Data.Has

import           Fission.IPFS.Types as IPFS
import           Fission.Web.Server
import qualified Fission.Web.IPFS.Peer   as Peer
import qualified Fission.Web.IPFS.Upload as Upload

type API = {- Root -} Upload.API
      :<|> "peers" :> Peer.API

server :: HasLogFunc cfg
       => HasProcessContext cfg
       => Has IPFS.Path cfg
       => RIOServer cfg API
server = Upload.add :<|> Peer.index
