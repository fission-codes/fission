{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web where

import RIO

import Servant
import Data.Has

import Fission.Config
import Fission.Web.Server

import qualified Fission.Web.IPFS as IPFS
import qualified Fission.Web.Ping as Ping

type API = "ping" :> Ping.API
      :<|> "ipfs" :> IPFS.API

app :: (Has IpfsPath cfg, HasLogFunc cfg) => cfg -> Application
app cfg = serve api $ hoistServer api (toHandler cfg) server

server :: (Has IpfsPath cfg, HasLogFunc cfg) => RIOServer cfg API
server = Ping.server :<|> IPFS.server

api :: Proxy API
api = Proxy
