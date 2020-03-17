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

import qualified Fission.Web.App.Create  as Create
import qualified Fission.Web.App.Destroy as Destroy

type API
  =    Create.API
  :<|> Destroy.API

server ::
  ( App.Domain.Initializer    m
  , MonadTime                 m
  , MonadDNSLink              m
  , MonadDB                 t m
  , App.Creator             t
  , App.Destroyer           t
  , App.Content.Initializer t
  )
  => Entity User
  -> ServerT API m
server user = Create.create user
         :<|> Destroy.server user
