module Fission.Web.App
  ( API
  , server
  ) where

import           Servant

import           Fission.Authorization
import           Fission.Prelude

import           Fission.IPFS.DNSLink.Class as DNSLink

import qualified Fission.App                as App
import qualified Fission.App.Content        as App.Content
import qualified Fission.App.Domain         as App.Domain

import qualified Fission.Web.App.Create     as Create
import qualified Fission.Web.App.Destroy    as Destroy
import qualified Fission.Web.App.Index      as Index
import qualified Fission.Web.App.Update     as Update

type API
  =    Index.API
  :<|> Create.API
  :<|> Update.API
  :<|> Destroy.API

server ::
  ( App.Domain.Initializer  m
  , App.CRUD                m
  , App.Content.Initializer m
  , MonadTime               m
  , MonadLogger             m
  , MonadDNSLink            m
  , MonadDB               t m
  , App.Retriever         t
  , App.Domain.Retriever  t
  )
  => Authorization
  -> ServerT API m
server auth = Index.index    auth
         :<|> Create.create  auth
         :<|> Update.update  auth
         :<|> Destroy.server auth
