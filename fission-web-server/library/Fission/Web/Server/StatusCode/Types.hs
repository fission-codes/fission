module Fission.Web.Server.StatusCode.Types (StatusCode (..)) where

import           Data.Has
import           Network.AWS.Route53
import           RIO

newtype StatusCode = StatusCode { unStatusCode :: Int }
  deriving newtype ( Show
                   , Eq
                   , Display
                   )

instance Has StatusCode ChangeResourceRecordSetsResponse where
  getter res = StatusCode $ res ^. crrsrsResponseStatus
  modifier updater resp = resp & crrsrsResponseStatus %~ mkInnerUpdater updater

instance Has StatusCode ListResourceRecordSetsResponse where
  getter res = StatusCode $ res ^. lrrsrsResponseStatus
  modifier updater resp = resp & lrrsrsResponseStatus %~ mkInnerUpdater updater

mkInnerUpdater :: (StatusCode -> StatusCode) -> (Int -> Int)
mkInnerUpdater updater = unStatusCode . updater . StatusCode
