module Fission.Web.User.Verify
  ( API
  , server
  ) where

import Servant

import Fission.Prelude

type API
  =  Summary "Validate auth"
  :> Description "DEPRECATED ⛔ Verify user auth -- prefer /user/whoami"
  :> Get '[JSON] Bool

server :: Monad m => ServerT API m
server = return True
