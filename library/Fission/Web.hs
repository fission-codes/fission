{-# LANGUAGE MonoLocalBinds #-}

-- | Top level web application and API
module Fission.Web
  ( API
  , app
  , server
  ) where

import           Servant
import           Network.IPFS

import           Fission.Prelude

import           Fission.Internal.Orphanage.OctetStream ()
import           Fission.Internal.Orphanage.PlainText   ()

import           Fission.IPFS.DNSLink          as DNSLink
import           Fission.IPFS.Linked
import           Fission.Platform.Heroku.AddOn as Heroku

import           Fission.Web.Server
import           Fission.Web.Server.Reflective

import qualified Fission.Web.Auth    as Auth
import qualified Fission.Web.DNS     as DNS
import qualified Fission.Web.Heroku  as Heroku
import qualified Fission.Web.IPFS    as IPFS
import qualified Fission.Web.Ping    as Ping
import qualified Fission.Web.Routes  as Web
import qualified Fission.Web.Swagger as Web.Swagger
import qualified Fission.Web.Types   as Web
import qualified Fission.Web.User    as User

-- | Top level web API type. Handled by 'server'.
type API = Web.Swagger.API :<|> Web.API

app ::
  ( MonadLocalIPFS        (RIO cfg)
  , MonadRemoteIPFS       (RIO cfg)
  , MonadLinkedIPFS       (RIO cfg)
  , MonadTime             (RIO cfg)
  , MonadDB               (RIO cfg)
  , MonadDNSLink          (RIO cfg)
  , MonadLogger           (RIO cfg)
  , MonadReflectiveServer (RIO cfg)
  , MonadReader                cfg m
  , MonadHerokuAddOn               m
  , MonadReflectiveServer          m
  )
  => m Application
app = do
  cfg     <- ask
  auth    <- mkAuth
  appHost <- getHost

  appHost
    |> server
    |> Auth.server api (toHandler cfg)
    |> serveWithContext api auth
    |> pure
  where
    api = Proxy @API

-- | Construct an authorization context
mkAuth ::
  ( MonadHerokuAddOn        m
  , MonadReader         cfg m
  , MonadDB        (RIO cfg)
  , MonadLogger    (RIO cfg)
  )
  => m (Context Auth.Checks)
mkAuth = do
  cfg        <- ask
  herokuAuth <- Heroku.authorize
  return <| Auth.user cfg
         :. herokuAuth
         :. EmptyContext

-- | Web handlers for the 'API'
server ::
  ( MonadDB               m
  , MonadTime             m
  , MonadLogger           m
  , MonadDNSLink          m
  , MonadLocalIPFS        m
  , MonadRemoteIPFS       m
  , MonadLinkedIPFS       m
  , MonadReflectiveServer m
  )
  => Web.Host
  -> ServerT API m
server appHost = Web.Swagger.server fromHandler appHost
            :<|> IPFS.server
            :<|> (\_ -> Heroku.server)
            :<|> User.server
            :<|> pure Ping.pong
            :<|> DNS.server
