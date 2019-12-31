module Fission.AWS.Route53.Class (MonadRoute53 (..)) where

import           Network.AWS
import           Network.AWS.Route53

import           Servant

import           Fission.AWS.Types as AWS
import           Fission.Prelude

class MonadAWS m => MonadRoute53 m where
  update :: RecordType -> AWS.DomainName -> Text -> m (Either ServerError ChangeResourceRecordSetsResponse)
