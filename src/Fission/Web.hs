{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web where

import RIO

import Servant

import Data.Has

import Fission.Config
import Fission.Web.Server
import qualified Fission.Web.Auth as Auth

import qualified Fission.Web.IPFS as IPFS
import qualified Fission.Web.Ping as Ping

type API = "ping" :> Ping.API
      :<|> "ipfs" :> Servant.BasicAuth "admin realm" Text {- TODO `User` -} :> IPFS.API

app :: (Has IpfsPath cfg, HasLogFunc cfg) => cfg -> Application
app cfg = serveWithContext api Auth.basic $ Auth.server api cfg server

server :: (Has IpfsPath cfg, HasLogFunc cfg) => RIOServer cfg API
server = Ping.server :<|> \_user -> IPFS.server

api :: Proxy API
api = Proxy
