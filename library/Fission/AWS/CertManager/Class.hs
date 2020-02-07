module Fission.AWS.CertManager.Class (MonadCertManager (..)) where

import           Fission.Prelude
import           Servant

import           Network.AWS
import           Network.AWS.CertificateManager

import           Fission.AWS.CertManager.Types

import           Fission.URL.Types as URL

class MonadAWS m => MonadCertManager m where
  requestCert  :: URL.DomainName -> m (Either ServerError CertARN)
  describeCert :: CertARN -> m (Either ServerError CertificateDetail)
