{-# LANGUAGE MonoLocalBinds #-}

-- | Top level web application and API
module Fission.Web
  ( API
  , app
  , server
  ) where

import           RIO
import           RIO.Process (HasProcessContext)

import           Data.Has
import           Database.Selda
import           Data.Swagger as Swagger
import qualified Network.HTTP.Client as HTTP
import           Servant

import qualified Fission.Config     as Config
import           Fission.User
import           Fission.Web.Server
import qualified Fission.IPFS.Types as IPFS
import           Fission.File.Types ()
import           Fission.Internal.Orphanage.PlainText ()
import           Fission.Internal.Orphanage.OctetStream ()

import qualified Fission.Web.User        as User
import qualified Fission.Web.Auth        as Auth
import qualified Fission.Web.IPFS        as IPFS
import qualified Fission.Web.Ping        as Ping
import qualified Fission.Web.DNS         as DNS

import qualified Fission.Web.Routes      as Web
import qualified Fission.Web.Swagger     as Web.Swagger
import qualified Fission.Web.Types       as Web

import qualified Network.AWS.Auth  as AWS
import qualified Fission.AWS.Types as AWS

import qualified Fission.Platform.Heroku.Types as Heroku
import qualified Fission.Web.Heroku            as Heroku

-- | Top level web API type. Handled by 'server'.
type API = Web.Swagger.API :<|> Web.API

-- | The actual web server for 'API'
app :: Has IPFS.BinPath    cfg
    => Has IPFS.Timeout    cfg
    => Has IPFS.URL        cfg
    => Has HTTP.Manager    cfg
    => Has Web.Host        cfg
    => Has AWS.AccessKey   cfg
    => Has AWS.SecretKey   cfg
    => Has AWS.ZoneID      cfg
    => Has AWS.DomainName  cfg
    => Has Heroku.ID       cfg
    => Has Heroku.Password cfg
    => HasProcessContext   cfg
    => HasLogFunc          cfg
    => MonadSelda     (RIO cfg)
    =>     cfg
    -> RIO cfg Application
app cfg = do
  auth               <- mkAuth
  appHost :: Web.Host <- Config.get
  return . serveWithContext api auth
         . Auth.server api cfg
         . server
         $ Swagger.Host (show appHost) Nothing
  where
    api :: Proxy API
    api = Proxy

-- | Construct an authorization context
mkAuth :: Has Heroku.ID       cfg
       => Has Heroku.Password cfg
       => HasLogFunc          cfg
       => MonadSelda     (RIO cfg)
       => RIO cfg (Context '[BasicAuthCheck User, BasicAuthCheck ByteString])
mkAuth = do
  Heroku.ID       hkuID   <- Config.get
  Heroku.Password hkuPass <- Config.get

  let hku = Auth.basic hkuID hkuPass
  usr <- Auth.user

  return $ usr
        :. hku
        :. EmptyContext

-- | Web handlers for the 'API'
server :: Has IPFS.BinPath   cfg
       => Has IPFS.Timeout   cfg
       => Has HTTP.Manager   cfg
       => Has IPFS.URL       cfg
       => Has Web.Host       cfg
       => Has AWS.AccessKey  cfg
       => Has AWS.SecretKey  cfg
       => Has AWS.ZoneID     cfg
       => Has AWS.DomainName cfg
       => HasProcessContext  cfg
       => HasLogFunc         cfg
       => MonadSelda    (RIO cfg)
       => Swagger.Host
       -> RIOServer          cfg API
server host' = Web.Swagger.server host'
          :<|> IPFS.server
          :<|> const Heroku.server
          :<|> User.server
          :<|> pure Ping.pong
          :<|> DNS.server
