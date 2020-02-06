module Fission.AWS.Route53.Class (MonadRoute53 (..)) where

import           Network.AWS
import           Network.AWS.Route53

import           Servant

import           Fission.URL.Types as URL
import           Fission.Prelude

class MonadAWS m => MonadRoute53 m where
  update :: RecordType -> URL.DomainName -> Text -> m (Either ServerError ChangeResourceRecordSetsResponse)
  createZone :: URL.DomainName -> m (Either ServerError CreateHostedZoneResponse)
