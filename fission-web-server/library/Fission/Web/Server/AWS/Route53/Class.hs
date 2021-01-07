module Fission.Web.Server.AWS.Route53.Class (MonadRoute53(..)) where

import           Network.AWS
import           Network.AWS.Route53

import           Servant

import           Fission.Prelude

import           Fission.URL
import qualified Fission.Web.Server.AWS.Zone.Types as AWS

-- | Low-level Route53 interface / no auth checks do not use directly
class MonadAWS m => MonadRoute53 m where
  set ::
       RecordType
    -> URL
    -> AWS.ZoneID
    -> NonEmpty Text
    -> Natural
    -> m (Either ServerError ChangeResourceRecordSetsResponse)

  get ::
       URL
    -> AWS.ZoneID
    -> m (Either ServerError ResourceRecordSet)

  clear ::
       RecordType
    -> URL
    -> AWS.ZoneID
    -> m (Either ServerError ChangeResourceRecordSetsResponse)
