module Fission.Web.User.Verify
  ( API
  , server
  ) where

import RIO

import Servant

import Fission.Web.Server

type API = Get '[JSON] Bool

server :: RIOServer cfg API
server = pure True
