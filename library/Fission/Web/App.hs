module Fission.Web.App
  ( API
  , server
  ) where

import           Database.Esqueleto
import           Servant

import           Fission.Prelude
import           Fission.Models

import           Fission.IPFS.DNSLink.Class as DNSLink
import           Fission.App.Domain.Class

import qualified Fission.App.Creator.Class   as App
import qualified Fission.App.Destroyer.Class as App

import qualified Fission.Web.App.Create  as Create
import qualified Fission.Web.App.Destroy as Destroy

type API
  =    Create.API
  :<|> Destroy.API

server ::
  ( HasBaseAppDomain m
  , MonadTime        m
  , MonadDNSLink     m
  , MonadDB        t m
  , App.Creator    t
  , App.Destroyer  t
  )
  => Entity User
  -> ServerT API m
server user = Create.create user
         :<|> Destroy.server user
