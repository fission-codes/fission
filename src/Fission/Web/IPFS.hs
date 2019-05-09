{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web.IPFS where

import RIO

import Servant

import           Fission.Config
import           Fission.Web.Internal
import qualified Fission.Web.IPFS.Peer   as Peer
import qualified Fission.Web.IPFS.Upload as Upload

type API = {- Root -} Upload.API
      :<|> "peers" :> Peer.API

server :: (HasLogFunc cfg, HasIPFSPath cfg) => RIOServer cfg API
server = Upload.server :<|> Peer.server

api :: Proxy API
api = Proxy
