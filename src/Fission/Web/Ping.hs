{-# LANGUAGE DataKinds #-}

module Fission.Web.Ping where

import RIO

import Network.Wai
import Servant

type API = Get '[JSON] Text

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return "pong"
