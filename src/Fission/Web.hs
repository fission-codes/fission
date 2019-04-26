{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Fission.Web where

import RIO

import Network.Wai
import Servant

import qualified Fission.Web.IPFS as IPFS
import qualified Fission.Web.Ping as Ping

type API = "ping" :> Ping.API
      :<|> "ipfs" :> IPFS.API

server :: Server API
server = Ping.server :<|> IPFS.server

app :: Application
app = serve api server

api :: Proxy API
api = Proxy
