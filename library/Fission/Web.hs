{-# LANGUAGE MonoLocalBinds #-}

-- | Top level web application and API
module Fission.Web
  ( API
  , app
  , server
  ) where

import           Servant

import           Network.IPFS
import qualified Network.IPFS.Types as IPFS

import           Fission.Prelude
import qualified Fission.Config     as Config

import           Fission.Internal.Orphanage.OctetStream ()
import           Fission.Internal.Orphanage.PlainText   ()

import           Fission.IPFS.DNSLink.Class as DNSLink

import           Fission.Web.Server
import qualified Fission.Web.Auth    as Auth
import qualified Fission.Web.DNS     as DNS
import qualified Fission.Web.Heroku  as Heroku
import qualified Fission.Web.IPFS    as IPFS
import qualified Fission.Web.Ping    as Ping
import qualified Fission.Web.Routes  as Web
import qualified Fission.Web.Swagger as Web.Swagger
import qualified Fission.Web.Types   as Web
import qualified Fission.Web.User    as User

import           Fission.Platform.Heroku.AddOn

-- | Top level web API type. Handled by 'server'.
type API = Web.Swagger.API :<|> Web.API

app ::
  ( MonadLocalIPFS      (RIO cfg)
  , MonadRemoteIPFS     (RIO cfg)
  , MonadTime           (RIO cfg)
  , MonadDNSLink        (RIO cfg)
  , MonadDB             (RIO cfg)
  , MonadHerokuAddOn             m
  , MonadReader              cfg m
  , Has IPFS.Peer            cfg
  , Has Web.Host             cfg
  , HasLogFunc               cfg
  )
  => m Application
app = do
  cfg     <- ask
  auth    <- mkAuth
  appHost <- Config.get

  appHost
    |> server
    |> Auth.server api (toHandler cfg)
    |> serveWithContext api auth
    |> pure
  where
    api = Proxy @API

-- | Construct an authorization context
mkAuth ::
  ( HasLogFunc          cfg
  , MonadReader         cfg m
  , MonadDB        (RIO cfg)
  , MonadHerokuAddOn m
  )
  => m (Context Auth.Checks)
mkAuth = do
  cfg        <- ask
  herokuAuth <- authorize
  return <| Auth.user cfg
         :. herokuAuth
         :. EmptyContext

-- | Web handlers for the 'API'
server ::
  ( MonadLocalIPFS       m
  , MonadRemoteIPFS      m
  , MonadLogger          m
  , MonadTime            m
  , MonadDB              m
  , MonadDNSLink         m
  , MonadReader      cfg m
  , Has IPFS.Peer    cfg
  , Has Web.Host     cfg
  )
  => Web.Host
  -> ServerT API m
server appHost = Web.Swagger.server fromHandler appHost
            :<|> IPFS.server
            :<|> const Heroku.server
            :<|> User.server
            :<|> pure Ping.pong
            :<|> DNS.server
