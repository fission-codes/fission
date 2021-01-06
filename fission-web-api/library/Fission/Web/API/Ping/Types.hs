module Fission.Web.API.Ping.Types (Ping) where

import           Fission.Web.API.Prelude

import           Fission.Web.API.Ping.Pong.Types

type Ping = "ping" :> Pong
