module Fission.Web.DNS.Create
  ( API
  , server
  ) where

import           Database.Esqueleto
import           Servant

import           Fission.Prelude
import           Fission.Models

import           Fission.AWS.Route53.Class  as Route53

import           Fission.Web.Error          as Web.Err
import qualified Fission.URL.Types          as URL

type API = Capture "domain" URL.DomainName
        :> PostAccepted '[PlainText, OctetStream] URL.DomainName

server :: MonadRoute53 m => Entity User -> ServerT API m
-- server (Entity _id User { userUsername }) domain =
server _user domain = do
  domain
    |> Route53.createZone
    |> bind Web.Err.ensureM

  return domain
