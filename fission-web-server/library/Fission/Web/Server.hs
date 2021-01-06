-- | Top level web application and API
module Fission.Web.Server
  ( API
  , app
  , server
  , bizServer
  , runServer
  ) where

import           Network.IPFS
import           Servant

import           Fission.Prelude

import           Fission.IPFS.Linked

import           Fission.Web.Handler
import           Fission.Web.Server.Reflective

import qualified Fission.App                                       as App
import qualified Fission.App.Content                               as App.Content
import qualified Fission.App.Domain                                as App.Domain

import qualified Fission.Platform.Heroku.AddOn                     as Heroku.AddOn
import qualified Fission.User                                      as User

import           Fission.WNFS

import qualified Fission.Challenge                                 as Challenge
import           Fission.Email

import qualified Fission.Web.Server.Handler.App                    as App
import qualified Fission.Web.Server.Handler.Auth                   as Auth
import qualified Fission.Web.Server.Handler.DNS                    as DNS
import qualified Fission.Web.Server.Handler.Heroku                 as Heroku
import qualified Fission.Web.Server.Handler.IPFS                   as IPFS
import qualified Fission.Web.Server.Handler.Ping                   as Ping
import qualified Fission.Web.Server.Handler.Swagger                as Web.Swagger
import qualified Fission.Web.Server.User                           as User

import           Fission.Web.Server.IPFS.DNSLink                   as DNSLink
import qualified Fission.Web.Server.LoosePin                       as LoosePin

import           Fission.Web.Server.Config.Types
import           Fission.Web.Server.Types

import           Fission.Web.Server.Internal.Orphanage.OctetStream ()
import           Fission.Web.Server.Internal.Orphanage.PlainText   ()

-- | Top level web API type. Handled by 'server'.
type API = Web.Swagger.API :<|> Web.API

-- | Run actions described by a @Fission@ type
runServer :: MonadIO m => cfg -> Server a -> m a
runServer cfg actions = runRIO cfg $ unServer actions

app ::
  ( App.Domain.Initializer    m
  , App.Content.Initializer   m
  , App.CRUD                  m
  , MonadReflectiveServer     m
  , MonadLinkedIPFS           m
  , MonadRemoteIPFS           m
  , MonadLocalIPFS            m
  , MonadDNSLink              m
  , MonadWNFS                 m
  , MonadLogger               m
  , MonadTime                 m
  , MonadEmail                m
  , User.CRUD                 m
  , Challenge.Creator         m
  , Challenge.Verifier        m
  , MonadDB                 t m
  , MonadLogger             t
  , MonadThrow              t
  , Heroku.AddOn.CRUD       t
  , LoosePin.CRUD           t
  , User.Retriever          t
  , User.Destroyer          t
  , App.Retriever           t
  , App.Domain.Retriever    t
  )
  => (forall a . m a -> Handler a)
  -> Context Auth.Checks
  -> Web.Host
  -> Application
app handlerNT authChecks appHost = do
  appHost
    |> server
    |> Auth.authWithContext api handlerNT
    |> serveWithContext     api authChecks
  where
    api = Proxy @API

-- | Web handlers for the 'API'
server ::
  ( App.Domain.Initializer    m
  , App.Content.Initializer   m
  , App.CRUD                  m
  , MonadReflectiveServer     m
  , MonadLinkedIPFS           m
  , MonadRemoteIPFS           m
  , MonadLocalIPFS            m
  , MonadDNSLink              m
  , MonadWNFS                 m
  , MonadLogger               m
  , MonadTime                 m
  , MonadEmail                m
  , User.CRUD                 m
  , Challenge.Creator         m
  , Challenge.Verifier        m
  , MonadDB                 t m
  , MonadLogger             t
  , MonadThrow              t
  , Heroku.AddOn.CRUD       t
  , LoosePin.CRUD           t
  , User.Retriever          t
  , User.Destroyer          t
  , App.Retriever           t
  , App.Domain.Retriever    t
  )
  => Web.Host
  -> ServerT API m
server appHost = Web.Swagger.server fromHandler appHost
            :<|> bizServer

bizServer ::
  ( App.Domain.Initializer    m
  , App.Content.Initializer   m
  , App.CRUD                  m
  , MonadReflectiveServer     m
  , MonadLinkedIPFS           m
  , MonadRemoteIPFS           m
  , MonadLocalIPFS            m
  , MonadDNSLink              m
  , MonadWNFS                 m
  , MonadLogger               m
  , MonadTime                 m
  , MonadEmail                m
  , User.CRUD                 m
  , Challenge.Creator         m
  , Challenge.Verifier        m
  , MonadDB                 t m
  , MonadLogger             t
  , MonadThrow              t
  , Heroku.AddOn.CRUD       t
  , LoosePin.CRUD           t
  , User.Retriever          t
  , User.Destroyer          t
  , App.Retriever           t
  , App.Domain.Retriever    t
  )
  => ServerT Web.API m
bizServer = IPFS.server
       :<|> App.server
       :<|> (\_ -> Heroku.server)
       :<|> User.server
       :<|> pure Ping.pong
       :<|> DNS.server
