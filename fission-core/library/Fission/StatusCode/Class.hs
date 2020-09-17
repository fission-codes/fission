module Fission.StatusCode.Class (HasStatusCode (..))
where

import           RIO
import           Network.AWS.Route53
import           Fission.StatusCode.Types


class HasStatusCode a where
  getStatusCode :: a -> StatusCode

instance HasStatusCode ChangeResourceRecordSetsResponse where
  getStatusCode res = StatusCode $ res ^. crrsrsResponseStatus

instance HasStatusCode ListResourceRecordSetsResponse where
  getStatusCode res = StatusCode $ res ^. lrrsrsResponseStatus
