{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web where

import RIO

import Servant

import Fission.Env
import Fission.Web.Internal

import qualified Fission.Web.IPFS as IPFS
import qualified Fission.Web.Ping as Ping

type API = "ping" :> Ping.API
      :<|> "ipfs" :> IPFS.API

app :: Env -> Application
app = serve api . toServer

toServer :: Env -> Server API
toServer env = hoistServer api (toHandler env) server

server :: FissionServer API
server = Ping.server :<|> IPFS.server

api :: Proxy API
api = Proxy
