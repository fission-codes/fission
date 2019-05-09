{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web where

import RIO

import Servant

import Fission.Config
import Fission.Web.Internal

import qualified Fission.Web.IPFS as IPFS
import qualified Fission.Web.Ping as Ping

type API = "ping" :> Ping.API
      :<|> "ipfs" :> IPFS.API

app :: (HasIPFSPath cfg, HasLogFunc cfg) => cfg -> Application
app cfg = serve api $ hoistServer api (toHandler cfg) server

server :: (HasIPFSPath cfg, HasLogFunc cfg) => RIOServer cfg API
server = Ping.server :<|> IPFS.server

api :: Proxy API
api = Proxy
