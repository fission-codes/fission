{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Fission.Web.Ping where

import RIO
import Servant

import Fission.Env
import Fission.Web.Internal

type API = Get '[JSON] Text

toServer :: Env -> Server API
toServer env = hoistServer api (toHandler env) server

api :: Proxy API
api = Proxy

server :: FissionServer API
server = return "pong"
