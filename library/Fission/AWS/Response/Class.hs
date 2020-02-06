module Fission.AWS.Response.Class (Response (..)) where

import RIO
import Network.AWS.Route53

class Response a where
  status :: a -> Int

instance Response ChangeResourceRecordSetsResponse where
  status changeSet = changeSet ^. crrsrsResponseStatus

instance Response CreateHostedZoneResponse where
  status changeSet = changeSet ^. chzrsResponseStatus
