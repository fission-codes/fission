module Fission.Web.App
  ( API
  , server
  ) where

import           Database.Esqueleto
import           Servant

import           Fission.Prelude
import           Fission.Models

import           Fission.IPFS.DNSLink.Class as DNSLink

import qualified Fission.App         as App
import qualified Fission.App.Content as App.Content
import qualified Fission.App.Domain  as App.Domain

import qualified Fission.Web.App.Index   as Index
import qualified Fission.Web.App.Create  as Create
import qualified Fission.Web.App.Update  as Update
import qualified Fission.Web.App.Destroy as Destroy

type API
  =    Index.API
  :<|> Create.API
  :<|> Update.API
  :<|> Destroy.API

server ::
  ( App.Domain.Initializer    m
  , MonadTime                 m
  , MonadLogger               m
  , MonadDNSLink              m
  , MonadDB                 t m
  , App.CRUD                t
  , App.Content.Initializer t
  , App.Domain.Retriever    t
  )
  => Entity User
  -> ServerT API m
server user = Index.index    user
         :<|> Create.create  user
         :<|> Update.update  user
         :<|> Destroy.server user
