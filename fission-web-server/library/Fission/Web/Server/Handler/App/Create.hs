module Fission.Web.Server.Handler.App.Create (create) where

import           Database.Esqueleto
import           Servant

import           Fission.Prelude

import           Fission.URL

import qualified Fission.Web.API.App.Create.Types       as API.App

import           Fission.Web.Server.Authorization.Types
import           Fission.Web.Server.Error               as Web.Error

import           Fission.Web.Server.App.Content         as AppCID
import qualified Fission.Web.Server.App.Creator         as App
import           Fission.Web.Server.App.Domain          as App.Domain

import           Fission.Web.Server.IPFS.DNSLink.Class  as DNSLink

create ::
  ( App.Domain.Initializer m
  , AppCID.Initializer     m
  , App.Creator            m
  , MonadTime              m
  , MonadLogger            m
  , MonadDNSLink           m
  )
  => ServerT API.App.Create m
create maySubdomain Authorization {about = Entity userId _} = do
  now            <- currentTime
  (_, subdomain) <- Web.Error.ensureM $ App.createWithPlaceholder userId maySubdomain now
  defaultDomain  <- App.Domain.initial

  return URL
    { domainName = defaultDomain
    , subdomain  = Just subdomain
    }
