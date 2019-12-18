module Fission.Web.User.Verify
  ( API
  , server
  ) where

import Servant

import Fission.Prelude

type API = Get '[JSON] Bool

server :: Applicative m => ServerT API m
server = pure True
