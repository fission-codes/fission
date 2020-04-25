module Fission.Web.App.Create
  ( API
  , create
  ) where

import           Database.Esqueleto
import           Servant

import           Fission.Prelude

import           Fission.Authorization
import           Fission.Web.Error as Web.Error
import           Fission.URL

import qualified Fission.App.Creator as App
import           Fission.App.Domain  as App.Domain
import           Fission.App.Content as App.Content

import           Fission.IPFS.DNSLink.Class as DNSLink

type API
  =  Summary "Create app"
  :> Description "Creates a new app, assigns an initial subdomain, and sets an asset placeholder"
  :> PostAccepted '[JSON] (Subdomain, DomainName)

create ::
  ( App.Domain.Initializer    m
  , MonadTime                 m
  , MonadLogger               m
  , MonadDNSLink              m
  , MonadDB                 t m
  , App.Creator             t
  , App.Content.Initializer t
  )
  => Authorization
  -> ServerT API m
create Authorization {about = Entity userId _} = do
  (_, subdomain) <- Web.Error.ensure =<< runDBNow (App.createWithPlaceholder userId)
  defaultDomain  <- App.Domain.initial
  return (subdomain, defaultDomain)
