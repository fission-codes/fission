module Fission.AWS.CertManager
  ( requestCert'
  , requestCertMock
  , describeCert'
  , describeCertMock
  , module Fission.AWS.CertManager.Class
  ) where

import           Fission.Prelude
import           RIO ((^.))
import           Servant

import           Network.AWS         as AWS
import           Network.AWS.CertificateManager

import           Fission.Web.Error
import           Fission.AWS.Error

import           Fission.AWS.CertManager.Types
import           Fission.AWS.CertManager.Error as CertManager
import           Fission.AWS.CertManager.Class

import qualified Fission.URL as URL

requestCert' :: 
  ( MonadAWS    m
  , MonadLogger m
  )
  => URL.DomainName
  -> m (Either ServerError CertARN)
requestCert' (URL.DomainName domain) = do
  logDebug <| "Requesting cert for domain: " <> displayShow domain

  res <- send <| requestCertificate domain

  return <| case validate res of 
    Left err -> Left err
    Right res' -> 
      case res' ^. rcrsCertificateARN of
        Nothing -> Left <| toServerError CertManager.NoARN
        Just arn -> Right <| CertARN arn

requestCertMock :: 
  ( MonadAWS    m
  , MonadLogger m
  )
  => URL.DomainName
  -> m (Either ServerError CertARN)
requestCertMock domain = do
  logDebug <| "MOCK: Request Cert for domain: " <> show domain
  return <| Right <| CertARN "mock-arn"

describeCert' :: 
  ( MonadAWS    m
  , MonadLogger m
  )
  => CertARN
  -> m (Either ServerError DescribeCertificateResponse)
describeCert' (CertARN arn) = do
  logDebug <| "Describing cert : " <> displayShow arn

  res <- send <| describeCertificate arn
  return <| validate res

describeCertMock :: 
  ( MonadAWS    m
  , MonadLogger m
  )
  => CertARN
  -> m (Either ServerError DescribeCertificateResponse)
describeCertMock arn = do
  logDebug <| "MOCK: Describing Cert for arn: " <> show arn
  let 
    mockStatus = 300
    mockRes    = describeCertificateResponse mockStatus
  return <| Right mockRes
