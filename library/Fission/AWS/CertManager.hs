module Fission.AWS.CertManager
  ( requestCert'
  , requestCertMock
  , describeCert'
  , describeCertMock
  , parseValidation
  , module Fission.AWS.CertManager.Class
  ) where

import           Fission.Prelude
import           RIO ((^.))
import           Control.Lens ((&))
import           Servant
import qualified Data.List.NonEmpty  as NonEmpty 

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

  let 
    req' = requestCertificate domain
    req = req' & rcValidationMethod ?~  DNS

  res <- send <| req

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
  -> m (Either ServerError CertificateDetail)
describeCert' (CertARN arn) = do
  logDebug <| "Describing cert : " <> displayShow arn

  res <- send <| describeCertificate arn
  logDebugN "blah 1"
  return <| case validate res of
    Left err -> Left err
    Right res' -> 
      case res' ^. dcrsCertificate of
        Nothing -> Left <| toServerError CertManager.CertNotFound
        Just details -> Right details

describeCertMock :: 
  ( MonadAWS    m
  , MonadLogger m
  )
  => CertARN
  -> m (Either ServerError CertificateDetail)
describeCertMock arn = do
  logDebug <| "MOCK: Describing Cert for arn: " <> show arn
  return <| Right certificateDetail

parseValidation :: MonadLogger m => CertificateDetail -> m ()
parseValidation details = do
  logDebugN "tick1"
  case details ^. cdDomainValidationOptions of
    Nothing -> logDebugN "no validation options"
    Just opts -> do
      logDebugN "tick2"
      let opt = NonEmpty.head opts
      case opt ^. dvResourceRecord of
        Nothing -> logDebugN "no resource record"
        Just record -> do
          logDebugN "tick3"
          logDebugN <| mconcat 
            [ "Resource record:"
            , "\nName: "
            , record ^. rrName
            , "\nType: "
            , textShow <| record ^. rrType
            , "\nValue: "
            , record ^. rrValue
            ]

  return ()
