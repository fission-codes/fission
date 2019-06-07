{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE MonoLocalBinds     #-}

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
import           Fission.User
import           Fission.Web.Server
import qualified Fission.Web.Auth as Auth

import qualified Fission.Web.IPFS as IPFS
import qualified Fission.Web.Ping as Ping
import qualified Fission.Web.Heroku as Heroku

type API = "ipfs"
             :> Servant.BasicAuth "registered users" User
             :> IPFS.API
      :<|> "heroku"
             :> Heroku.API
      :<|> "ping"
             :> Ping.API

app :: Has IpfsPath cfg
    => HasLogFunc cfg
    => Has Host cfg
    => MonadSelda (RIO cfg)
    -- => MonadSelda IO
    => cfg -> Application
app cfg = serveWithContext api Auth.user $ Auth.server api cfg server

server :: Has IpfsPath cfg
       => Has Host cfg
       => HasLogFunc cfg
       => MonadSelda (RIO cfg)
       => RIOServer cfg API
server = const IPFS.server
    :<|> Heroku.create
    :<|> Ping.server

api :: Proxy API
api = Proxy
