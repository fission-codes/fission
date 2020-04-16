module Fission.Web.User.WhoAmI
  ( API
  , server
  ) where

import Servant

import Fission.Prelude
import Fission.Authorization

import Fission.Models
import Fission.User.Username.Types

type API
  =  Summary "Get username"
  :> Description "Get username registered to currently authenticated user"
  :> Get '[PlainText, JSON] Username

server :: Monad m => Authorization -> ServerT API m
server Authorization {about = Entity _ User { userUsername }} =
  return userUsername
