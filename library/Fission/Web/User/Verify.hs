module Fission.Web.User.Verify
  ( API
  , server
  ) where

import Servant

import Fission.Prelude
import Fission.Web.Server

type API = Get '[JSON] Bool

server :: RIOServer cfg API
server = pure True
