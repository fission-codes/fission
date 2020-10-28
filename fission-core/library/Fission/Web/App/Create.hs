module Fission.Web.App.Create
  ( API
  , create
  ) where

import           Database.Esqueleto
import           Servant

import           Fission.Prelude

import           Fission.Authorization
import           Fission.URL
import           Fission.Web.Error          as Web.Error

-- import           Fission.Web.Auth.Token.UCAN.Resource.Types

import qualified Fission.Authorization      as Authorization

import           Fission.App.Content        as AppCID
import qualified Fission.App.Creator        as App
import           Fission.App.Domain         as App.Domain

import           Fission.IPFS.DNSLink.Class as DNSLink

type API
  =  Summary "Create app"
  :> Description "Creates a new app, assigns an initial subdomain, and sets an asset placeholder"
  :> QueryParam "subdomain" Subdomain
  :> PostAccepted '[JSON] URL

create ::
  ( App.Domain.Initializer m
  , AppCID.Initializer     m
  , App.Creator            m
  , MonadTime              m
  , MonadLogger            m
  , MonadDNSLink           m
  )
  => Authorization.Session
  -> ServerT API m
create Authorization.Session {} maySubdomain = do
-- create Authorization.Session {about = Entity userId _} maySubdomain = do

  let userId = undefined -- FIXME

  now            <- currentTime
  (_, subdomain) <- Web.Error.ensureM $ App.createWithPlaceholder userId maySubdomain now
  defaultDomain  <- App.Domain.initial

  return URL
    { domainName = defaultDomain
    , subdomain  = Just subdomain
    }
