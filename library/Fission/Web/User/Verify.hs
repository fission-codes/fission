module Fission.Web.User.Verify
  ( API
  , server
  ) where

import Servant

import Fission.Prelude
import Fission.Models

import Fission.User.Username.Types

import Database.Esqueleto

type API = Get '[PlainText] Username

server ::
  Monad m
  => Entity User
  -> ServerT API m
server (Entity _ User { userUsername }) = 
  return <| Username userUsername
