module Fission.StatusCode.Types (StatusCode (..)) where

import           Control.Lens        ((%~))
import           Data.Has
import           Network.AWS.Route53
import           RIO


newtype StatusCode = StatusCode { unStatusCode :: Int }
  deriving newtype ( Show
                   , Eq
                   , Display
                   )


statusCodeLens = lens unStatusCode (\_ x -> StatusCode x)

instance Has StatusCode ChangeResourceRecordSetsResponse where
  hasLens = crrsrsResponseStatus . statusCodeLens

  -- getter res = StatusCode $ res ^. crrsrsResponseStatus
  -- modifier fn res = res & crrsrsResponseStatus %~ (fn (getter res))

-- instance Has StatusCode ListResourceRecordSetsResponse where
--   -- hasLens = lrrsrsResponseStatus
--   getter res = Statuscode $ res ^. lrrsrsResponseStatus

