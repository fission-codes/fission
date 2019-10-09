module Fission.Web.Ping
  ( API
  , pong
  ) where

import Servant
import Fission.Web.Ping.Types

type API = Get '[JSON, PlainText] Pong

pong :: Pong
pong = Pong "pong"
