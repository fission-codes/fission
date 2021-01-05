module Fission.Web.Server.Handler.App (server) where

import           Servant

import           Fission.Authorization
import           Fission.Prelude

import qualified Fission.App                           as App
import qualified Fission.App.Content                   as App.Content
import qualified Fission.App.Domain                    as App.Domain

import           Fission.Web.Server.IPFS.DNSLink.Class as DNSLink

import qualified Fission.Web.Server.App.Create         as Create
import qualified Fission.Web.Server.App.Destroy        as Destroy
import qualified Fission.Web.Server.App.Index          as Index
import qualified Fission.Web.Server.App.Update         as Update

handler ::
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
handler auth = Index.index    auth
          :<|> Create.create  auth
          :<|> Update.update  auth
          :<|> Destroy.server auth
