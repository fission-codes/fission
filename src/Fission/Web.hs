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
import Fission.Web.Auth

import qualified Fission.Web.IPFS as IPFS
import qualified Fission.Web.Ping as Ping

type API = "ping" :> Ping.API
      :<|> "ipfs" :> Servant.BasicAuth "admin realm" Text {- TODO `User` -} :> IPFS.API

app :: (Has IpfsPath cfg, HasLogFunc cfg) => cfg -> Application
app cfg = serveWithContext api basicAuthContext $ protectedServer api cfg server

server :: (Has IpfsPath cfg, HasLogFunc cfg) => RIOServer cfg API
server = Ping.server :<|> guarded IPFS.server

api :: Proxy API
api = Proxy

guarded :: Has IpfsPath cfg => RIOServer cfg IPFS.API -> Text -> RIOServer cfg IPFS.API
guarded s "password" = s
guarded _ _          = error "boom" -- throwM err401
