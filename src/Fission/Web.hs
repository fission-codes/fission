{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web
  ( API
  , app
  , server
  ) where

import RIO

import Servant

import Data.Has
import Database.Selda

import Fission.Config
import Fission.Web.Server
import qualified Fission.Web.Auth as Auth

import qualified Fission.Web.IPFS as IPFS
import qualified Fission.Web.Ping as Ping
import qualified Fission.Web.Heroku as Heroku

type API = "ping"
             :> Ping.API
      :<|> "ipfs"
             :> Servant.BasicAuth "admin realm" ByteString {- TODO `User` -}
             :> IPFS.API
      :<|> "heroku"
             :> Heroku.API

app :: Has IpfsPath cfg
    => Has AuthUsername cfg
    => Has AuthPassword cfg
    => HasLogFunc cfg
    => MonadSelda (RIO cfg)
    => cfg
    -> Application
app cfg =
  serveWithContext api authCtx $ Auth.server api cfg server
  where
    AuthUsername unOK = cfg ^. hasLens
    AuthPassword pwOK = cfg ^. hasLens
    authCtx           = Auth.basic unOK pwOK

server :: Has IpfsPath cfg
       => HasLogFunc cfg
       => MonadSelda (RIO cfg)
       => RIOServer cfg API
server = Ping.server
  :<|> const IPFS.server {- TODO use `User` -}
  :<|> Heroku.server

api :: Proxy API
api = Proxy
