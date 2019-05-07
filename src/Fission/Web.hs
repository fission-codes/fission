{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web where

import RIO

import Network.Wai
import Servant

import Fission.Env
import Fission.Web.Types

import qualified Fission.Web.IPFS as IPFS
import qualified Fission.Web.Ping as Ping

type API = "ping" :> Ping.API
      :<|> "ipfs" :> IPFS.API

server :: FissionServer API -- Env -> Server API
server = Ping.server :<|> IPFS.serverT

-- app :: Env -> Application
-- app env = serve api $ server env

api :: Proxy API
api = Proxy

nt :: ServerT api Servant.Handler -> ServerT api Fission
nt server =
