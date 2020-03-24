module Fission.Web.User.Verify
  ( API
  , server
  ) where

import Servant

import Fission.Prelude

type API
  =  Summary "[DEPRECATED] Verify user auth"
  :> Get '[JSON] Bool

server :: Monad m => ServerT API m
server = return True
