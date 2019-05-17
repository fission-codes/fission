{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Fission.Web.Ping
  ( API
  , server
  ) where

import RIO
import Servant

import Fission.Web.Server

type API = Get '[JSON, PlainText] Text

server :: RIOServer cfg API
server = return "pong"
