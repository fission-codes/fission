module Fission.Web.Ping
  ( API
  , pong
  ) where

import RIO
import Servant

type API = Get '[JSON, PlainText] Text

pong :: Text
pong = "pong"
