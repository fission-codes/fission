module Fission.Web.API.Ping.Pong.Types (Pong) where

import           Fission.Web.API.Prelude

import qualified Fission.Pong.Types      as Ping

type Pong
  = Summary "Simple Ping"
  :> Description "A quick way to check for liveness"
  --
  :> Get '[JSON, PlainText] Ping.Pong
