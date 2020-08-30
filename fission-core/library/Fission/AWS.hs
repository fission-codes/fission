module Fission.AWS
  ( validate
  , module Fission.AWS.Types
  , module Fission.AWS.Route53
  ) where

import RIO

import Servant
import Network.AWS.Route53

import Fission.Web.Error

import Fission.AWS.Route53
import Fission.AWS.Types

validate :: ChangeResourceRecordSetsResponse -> Either ServerError ChangeResourceRecordSetsResponse
validate changeSet =
  if status >= 300
    then Left (toServerError status)
    else Right changeSet

  where
    status = changeSet ^. crrsrsResponseStatus
