module Fission.Web.Server.Handler.App (handler) where

import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.App.Types              as API

import           Fission.Web.Server.IPFS.DNSLink.Class  as DNSLink
import           Fission.Web.Server.MonadDB

import qualified Fission.Web.Server.App                 as App
import qualified Fission.Web.Server.App.Content         as App.Content
import qualified Fission.Web.Server.App.Domain          as App.Domain

import qualified Fission.Web.Server.Handler.App.Create  as Create
import qualified Fission.Web.Server.Handler.App.Destroy as Destroy
import qualified Fission.Web.Server.Handler.App.Index   as Index
import qualified Fission.Web.Server.Handler.App.Update  as Update



import           Fission.Web.Server.IPFS.Cluster.Class
import           Network.IPFS.Client.Streaming.Pin

handler ::
  ( MonadIPFSCluster m PinStatus -- FIXME abstract out
  , App.Domain.Initializer  m
  , App.CRUD                m
  , App.Content.Initializer m
  , MonadTime               m
  , MonadLogger             m
  , MonadDNSLink            m
  , MonadDB               t m
  , App.Retriever         t
  , App.Domain.Retriever  t
  )
  => ServerT API.App m
handler = Index.index
     :<|> Create.create
     :<|> Update.update
     :<|> Update.updateStreaming
     :<|> Destroy.handler
