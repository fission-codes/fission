{-# LANGUAGE MonoLocalBinds    #-}

module Fission.Web
  ( API
  , app
  , server
  ) where

import RIO

import Data.Has
import Database.Selda
import Servant

import           Fission.Config
import           Fission.User

import           Fission.Web.Server
import qualified Fission.Web.Auth   as Auth
import qualified Fission.Web.IPFS   as IPFS
import qualified Fission.Web.Ping   as Ping
import qualified Fission.Web.Heroku as Heroku

type API = "ipfs"
             :> Servant.BasicAuth "registered users" User
             :> IPFS.API
      :<|> "heroku"
             :> Heroku.API
      :<|> "ping"
             :> Ping.API

app :: Has IPFSPath cfg
    => HasLogFunc cfg
    => Has Host cfg
    => MonadSelda (RIO cfg)
    => cfg -> RIO cfg Application
app cfg = do
  auth <- Auth.user
  return . serveWithContext api auth
         $ Auth.server api cfg server

server :: Has IPFSPath cfg
       => Has Host cfg
       => HasLogFunc cfg
       => MonadSelda (RIO cfg)
       => RIOServer cfg API
server = const IPFS.server
    :<|> Heroku.create
    :<|> Ping.server

api :: Proxy API
api = Proxy
