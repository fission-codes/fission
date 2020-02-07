module Fission.Web.Cert
  ( API
  , server
  ) where

import           Database.Esqueleto
import           Servant

import           Fission.Prelude
import           Fission.Models

import           Fission.AWS.CertManager.Class  as CertManager

import           Fission.Web.Error          as Web.Err
import qualified Fission.URL.Types          as URL

type API = Capture "domain" URL.DomainName
        :> PostAccepted '[PlainText, OctetStream] URL.DomainName

server :: (MonadLogger m, MonadCertManager m) => Entity User -> ServerT API m
-- server (Entity _id User { userUsername }) domain =
server _user domain = do
  logDebugN <| "HERE"
  domain
    |> CertManager.requestCert
    |> bind Web.Err.ensureM

  return domain
