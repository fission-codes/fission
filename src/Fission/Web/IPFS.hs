{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web.IPFS where

import RIO

import Servant
import Data.Has

import           Fission.Config
import           Fission.Web.Internal
import qualified Fission.Web.IPFS.Peer   as Peer
import qualified Fission.Web.IPFS.Upload as Upload
import qualified Fission.Web.IPFS.Multipart as Multipart

type API = {- Root -} Upload.API
      :<|> "peers" :> Peer.API
      :<|> "file" :> Multipart.API

server :: (HasLogFunc cfg, Has IpfsPath cfg) => RIOServer cfg API
server = Upload.server :<|> Peer.server :<|> Multipart.server

api :: Proxy API
api = Proxy
