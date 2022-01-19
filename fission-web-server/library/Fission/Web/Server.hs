-- | Top level web application and API
module Fission.Web.Server (app, server) where

import           Network.IPFS
import           Network.IPFS.Client.Streaming.Pin

import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic

import qualified Web.UCAN.Resolver                            as Proof

import           Fission.Prelude

import qualified Fission.Web.API.Host.Types                   as Web
import qualified Fission.Web.API.Types                        as Fission

import qualified Fission.Web.Server.Auth                      as Auth
import qualified Fission.Web.Server.Challenge                 as Challenge
import           Fission.Web.Server.Email
import qualified Fission.Web.Server.RecoveryChallenge         as RecoveryChallenge
import qualified Fission.Web.Server.Swagger                   as Web.Swagger
import qualified Fission.Web.Server.User                      as User
import           Fission.Web.Server.WNFS

import qualified Fission.Web.Server.Handler.App               as App
import qualified Fission.Web.Server.Handler.DNS               as DNS
import qualified Fission.Web.Server.Handler.Heroku            as Heroku
import qualified Fission.Web.Server.Handler.IPFS              as IPFS
import qualified Fission.Web.Server.Handler.Ping              as Ping
import qualified Fission.Web.Server.Handler.User              as User
import qualified Fission.Web.Server.Heroku.AddOn              as Heroku.AddOn

import           Fission.Web.Server.IPFS.DNSLink              as DNSLink
import qualified Fission.Web.Server.LoosePin                  as LoosePin

import qualified Fission.Web.Server.App                       as App
import qualified Fission.Web.Server.App.Content               as App.Content
import qualified Fission.Web.Server.App.Domain                as App.Domain

import           Fission.Web.Server.Handler
import qualified Fission.Web.Server.Handler.Auth.UCAN         as Auth.UCAN

import           Fission.Web.Server.IPFS.Cluster              as Cluster
import           Fission.Web.Server.IPFS.Linked
import           Fission.Web.Server.MonadDB
import           Fission.Web.Server.Reflective
import           Fission.Web.Server.Relay.Store.Class

import           Fission.Internal.Orphanage.OctetStream       ()
import           Fission.Internal.Orphanage.PlainText         ()
import           Fission.Web.API.Internal.Orphanage.Aeson     ()
import           Fission.Web.API.Internal.Orphanage.SwaggerUi ()
import           Fission.Web.API.Internal.Orphanage.WebSocket ()

import qualified Paths_fission_web_server                     as Fission

-- | Top level web API type. Handled by 'server'.
app :: forall m t .
  ( App.Domain.Initializer      m
  , App.Content.Initializer     m
  , App.CRUD                    m
  , Proof.Resolver              m
  , MonadReflectiveServer       m
  , MonadRelayStore             m
  , MonadLinkedIPFS             m
  , MonadIPFSCluster            m PinStatus
  , MonadRemoteIPFS             m
  , MonadDNSLink                m
  , MonadWNFS                   m
  , MonadLogger                 m
  , MonadTime                   m
  , MonadEmail                  m
  , User.CRUD                   m
  , Challenge.Creator           m
  , Challenge.Retriever         m
  , Challenge.Verifier          m
  , RecoveryChallenge.Creator   m
  , RecoveryChallenge.Retriever m
  , RecoveryChallenge.Destroyer m
  , MonadDB                   t m
  , MonadLogger               t
  , MonadThrow                t
  , Heroku.AddOn.CRUD         t
  , LoosePin.CRUD             t
  , User.Retriever            t
  , User.Destroyer            t
  , App.Retriever             t
  , App.Domain.Retriever      t
  )
  => (forall a . m a -> Handler a)
  -> Context Auth.Checks
  -> Web.Host
  -> Application
app handlerNT authChecks appHost =
  appHost
    |> server
    |> genericServerT
    |> Auth.authWithContext api handlerNT
    |> serveWithContext     api authChecks
  where
    api = Proxy @(ToServantApi Fission.Routes)

-- | Web handlers for the 'API'
server ::
  ( App.Domain.Initializer      m
  , App.Content.Initializer     m
  , App.CRUD                    m
  , Proof.Resolver              m
  , MonadReflectiveServer       m
  , MonadRelayStore             m
  , MonadLinkedIPFS             m
  , MonadRemoteIPFS             m
  , MonadIPFSCluster            m PinStatus
  , MonadDNSLink                m
  , MonadWNFS                   m
  , MonadLogger                 m
  , MonadTime                   m
  , MonadEmail                  m
  , User.CRUD                   m
  , Challenge.Creator           m
  , Challenge.Retriever         m
  , Challenge.Verifier          m
  , RecoveryChallenge.Creator   m
  , RecoveryChallenge.Retriever m
  , RecoveryChallenge.Destroyer m
  , MonadDB                   t m
  , MonadLogger               t
  , MonadThrow                t
  , Heroku.AddOn.CRUD         t
  , LoosePin.CRUD             t
  , User.Retriever            t
  , User.Destroyer            t
  , App.Retriever             t
  , App.Domain.Retriever      t
  )
  => Web.Host
  -> Fission.Routes (AsServerT m)
server appHost =
  Fission.Routes
    { v2
    , heroku      = genericServerT Heroku.handler
    , latestDocs  = v2Docs
    , unversioned = serverV_
    , root        = pure NoContent
    }

  where
    v2 =
      genericServerT Fission.RoutesV2
        { api  = serverV2
        , docs = v2Docs
        }

    serverV2 =
      genericServerT Fission.V2
        { ipfs   = genericServerT IPFS.handlerV2
        , app    = genericServerT App.handlerV2
        , user   = genericServerT User.handlerV2
        , auth   = genericServerT Auth.UCAN.handler
      }

    serverV_ =
      genericServerT Fission.RoutesV_
        { ipfs       = genericServerT IPFS.handlerV_
        , app        = genericServerT App.handlerV_
        , user       = genericServerT User.handlerV_
        , dns        = genericServerT DNS.handler
        , auth       = genericServerT Auth.UCAN.handler
        , ping       = Ping.handler
      }

    v2Docs =
      Web.Swagger.handler fromHandler appHost Fission.version (Proxy @("v2" :> (ToServantApi Fission.RoutesV2)))
