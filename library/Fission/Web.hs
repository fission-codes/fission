-- | Top level web application and API
module Fission.Web
  ( API
  , app
  , server
  ) where

import           Network.IPFS
import           Servant

import           Fission.Prelude

import           Fission.Internal.Orphanage.OctetStream ()
import           Fission.Internal.Orphanage.PlainText   ()

import           Fission.IPFS.DNSLink as DNSLink
import           Fission.IPFS.Linked

import           Fission.Web.Handler
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
-- REMove
import Fission.Storage
import           Fission.Models
-- | Top level web API type. Handled by 'server'.
type API = Web.Swagger.API :<|> Web.API

app ::
  ( MonadDBQuery UserCid      m
  , MonadDBQuery User         m
  , MonadDBQuery HerokuAddOn  m
  , MonadDBMutation User      m
  , MonadTime                 m
  , MonadLogger               m
  , MonadDNSLink              m
  , MonadLocalIPFS            m
  , MonadRemoteIPFS           m
  , MonadLinkedIPFS           m
  , MonadReflectiveServer     m
  )
  => (forall a . m a -> Handler a)
  -> Context Auth.Checks
  -> Web.Host
  -> Application
app handlerNT auth appHost = do
  appHost
    |> server
    |> Auth.server      api handlerNT
    |> serveWithContext api auth
  where
    api = Proxy @API

-- | Web handlers for the 'API'
server ::
  ( MonadDBQuery UserCid      m
  , MonadDBQuery User         m
  , MonadDBQuery HerokuAddOn  m
  , MonadDBMutation User      m
  , MonadTime                 m
  , MonadLogger               m
  , MonadDNSLink              m
  , MonadLocalIPFS            m
  , MonadRemoteIPFS           m
  , MonadLinkedIPFS           m
  , MonadReflectiveServer     m
  )
  => Web.Host
  -> ServerT API m
server appHost = Web.Swagger.server fromHandler appHost
            :<|> IPFS.server
            :<|> (\_ -> Heroku.server)
            :<|> User.server
            :<|> pure Ping.pong
            :<|> DNS.server
