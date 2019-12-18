{-# LANGUAGE MonoLocalBinds #-}

-- | Top level web application and API
module Fission.Web
  ( API
  , app
  , server
  ) where

import           Servant

import qualified Network.AWS.Auth   as AWS
import           Network.IPFS
import qualified Network.IPFS.Types as IPFS

import           Fission.Prelude
import qualified Fission.Config     as Config

import           Fission.Internal.Orphanage.OctetStream ()
import           Fission.Internal.Orphanage.PlainText   ()

import qualified Fission.Platform.Heroku.ID.Types       as Heroku
import qualified Fission.Platform.Heroku.Password.Types as Heroku

import qualified Fission.AWS.Types   as AWS

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

-- | Top level web API type. Handled by 'server'.
type API = Web.Swagger.API :<|> Web.API

app ::
  ( MonadLocalIPFS         (RIO cfg)
  , MonadRemoteIPFS        (RIO cfg)
  , MonadTime              (RIO cfg)
  , MonadDB                (RIO cfg)
  , MonadDB                         m
  , MonadReader                 cfg m
  , Has IPFS.Gateway            cfg
  , Has IPFS.Peer               cfg
  , Has Web.Host                cfg
  , Has AWS.AccessKey           cfg
  , Has AWS.SecretKey           cfg
  , Has AWS.ZoneID              cfg
  , Has AWS.DomainName          cfg
  , Has Heroku.ID               cfg
  , Has Heroku.Password         cfg
  , Has AWS.Route53MockEnabled  cfg
  , HasLogFunc                  cfg
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
  ( Has Heroku.ID       cfg
  , Has Heroku.Password cfg
  , HasLogFunc          cfg
  , MonadReader         cfg m
  , MonadDB        (RIO cfg)
  )
  => m (Context Auth.Checks)
mkAuth = do
  Heroku.ID       hkuID   <- Config.get
  Heroku.Password hkuPass <- Config.get
  cfg <- ask

  return <| Auth.user cfg
         :. Auth.basic hkuID hkuPass
         :. EmptyContext

-- | Web handlers for the 'API'
server ::
  ( MonadLocalIPFS  m
  , MonadRemoteIPFS m
  , MonadUnliftIO   m
  , MonadLogger     m
  , MonadThrow      m
  , MonadTime       m
  , MonadDB         m
  , MonadReader                cfg m
  , Has AWS.Route53MockEnabled cfg
  , Has IPFS.Gateway           cfg
  , Has IPFS.Peer              cfg
  , Has Web.Host               cfg
  , Has AWS.AccessKey          cfg
  , Has AWS.SecretKey          cfg
  , Has AWS.ZoneID             cfg
  , Has AWS.DomainName         cfg
  )
  => Web.Host
  -> ServerT API m
server appHost = Web.Swagger.server fromHandler appHost
            :<|> IPFS.server
            :<|> const Heroku.server
            :<|> User.server
            :<|> pure Ping.pong
            :<|> DNS.server
