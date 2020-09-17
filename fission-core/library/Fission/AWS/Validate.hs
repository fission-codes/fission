module Fission.AWS.Validate
  ( validate
  , HasStatus(..)
  )
where

import           RIO

import           Servant
import           Network.AWS.Route53

import           Fission.Web.Error


validate :: HasStatus a => a -> Either ServerError a
validate res = if status >= 300 then Left (toServerError status) else Right res
  where status = getStatus res

class HasStatus a where
  getStatus :: a -> Int

instance HasStatus ChangeResourceRecordSetsResponse where
  getStatus res = res ^. crrsrsResponseStatus

instance HasStatus ListResourceRecordSetsResponse where
  getStatus res = res ^. lrrsrsResponseStatus
