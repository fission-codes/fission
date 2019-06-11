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
             :> Servant.BasicAuth "heroku add-on api" ByteString
             :> Heroku.API
      :<|> "ping"
             :> Ping.API

app :: Has IPFSPath cfg
    => Has Host cfg
    => Has HerokuID cfg
    => Has HerokuPassword cfg
    => HasLogFunc cfg
    => MonadSelda (RIO cfg)
    => cfg
    -> RIO cfg Application
app cfg = do
  auth' <- auth
  return . serveWithContext api auth'
         $ Auth.server api cfg server

auth :: Has HerokuID cfg
     => Has HerokuPassword cfg
     => HasLogFunc cfg
     => MonadSelda (RIO cfg)
     => RIO cfg (Context '[BasicAuthCheck User, BasicAuthCheck ByteString])
auth = do
  HerokuID       hkuID   <- fromCfg
  HerokuPassword hkuPass <- fromCfg

  hku <- return $ Auth.basic hkuID hkuPass
  usr <- Auth.user

  return $ usr
        :. hku
        :. EmptyContext

server :: Has IPFSPath cfg
       => Has Host cfg
       => Has HerokuID cfg
       => Has HerokuPassword cfg
       => HasLogFunc cfg
       => MonadSelda (RIO cfg)
       => RIOServer cfg API
server = const IPFS.server
    :<|> const Heroku.create
    :<|> Ping.server

api :: Proxy API
api = Proxy
