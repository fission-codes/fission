module Fission.Web.Cert
  ( API
  , server
  ) where

import           Database.Esqueleto
import           Servant

import           Fission.Prelude
import           Fission.Models

import           Fission.AWS.CertManager  as CertManager
import           Fission.AWS.CertManager.Types

import           Fission.Web.Error          as Web.Err
import qualified Fission.URL.Types          as URL

type API = Capture "domain" URL.DomainName
        :> PostAccepted '[PlainText, OctetStream] URL.DomainName

server :: (MonadLogger m, MonadCertManager m) => Entity User -> ServerT API m
-- server (Entity _id User { userUsername }) domain =
server _user domain = do
  logDebugN <| "HERE"

  -- arn <- Web.Err.ensureM =<< CertManager.requestCert domain 
  let arn = CertARN "arn:aws:acm:us-east-1:348544643304:certificate/6986e821-db8d-4127-b326-fde8ff6d010c"
  details <- Web.Err.ensureM =<< CertManager.describeCert arn
  logDebugN "tick0"
  _ <- CertManager.parseValidation details

  return domain
