{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web.IPFS where

import RIO

import Network.Wai
import Servant

import Fission.IPFS.Peer as Peer

type API = "peers" :> Get '[JSON] [Peer]

app :: Application
app = serve api server

server :: Server API
server = liftIO Peer.all

api :: Proxy API
api = Proxy
