module Fission.Web.App.Create
  ( API
  , create
  ) where

import           Database.Esqueleto
import           Servant

import           Fission.Prelude
import           Fission.Web.Error
import           Fission.Models
import           Fission.URL

import qualified Fission.App.Creator as App
import           Fission.App.Domain  as App.Domain
import           Fission.App.Content as App.Content

import           Fission.IPFS.DNSLink.Class as DNSLink

type API = PostAccepted '[JSON] (Subdomain, DomainName)

create ::
  ( App.Domain.Initializer    m
  , MonadTime                 m
  , MonadDNSLink              m
  , MonadDB                 t m
  , App.Creator             t
  , App.Content.Initializer t
  )
  => Entity User
  -> ServerT API m
create (Entity userId _) = do
  defaultDomain <- App.Domain.initial

  userId
    |> App.createWithPlaceholder
    |> runDBNow
    |> bind \case
      Right (_, subdomain) -> return (subdomain, defaultDomain)
      Left err             -> throwM <| toServerError err
