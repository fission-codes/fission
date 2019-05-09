{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Fission.Web.Ping where

import RIO
import Servant

import Fission.Config
import Fission.Web.Internal

type API = Get '[JSON] Text

toServer :: Config -> Server API
toServer cfg = hoistServer api (toHandler cfg) server

api :: Proxy API
api = Proxy

server :: FissionServer API
server = return "pong"
