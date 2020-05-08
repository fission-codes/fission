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
import           Fission.App.Content as AppCID

import           Fission.IPFS.DNSLink.Class as DNSLink

type API
  =  Summary "Create app"
  :> Description "Creates a new app, assigns an initial subdomain, and sets an asset placeholder"
  :> PostAccepted '[JSON] URL

create ::
  ( App.Domain.Initializer m
  , AppCID.Initializer     m
  , App.Creator            m
  , MonadTime              m
  , MonadLogger            m
  , MonadDNSLink           m
  )
  => Authorization
  -> ServerT API m
create Authorization {about = Entity userId _} = do
  now            <- currentTime
  (_, subdomain) <- Web.Error.ensureM $ App.createWithPlaceholder userId now
  defaultDomain  <- App.Domain.initial
 
  return URL
    { domainName = defaultDomain
    , subdomain  = Just subdomain
    }
