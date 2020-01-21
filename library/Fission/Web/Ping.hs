module Fission.Web.Ping
  ( module Fission.Web.Ping.Types
  , API
  , pong
  ) where

import Servant
import Fission.Web.Ping.Types

type API = Get '[JSON, PlainText] Pong

pong :: Pong
pong = Pong "pong"
