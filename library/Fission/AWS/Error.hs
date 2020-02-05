module Fission.AWS.Error (validate) where

import RIO

import Servant
import Network.AWS.Route53

import Fission.Web.Error

validate :: ChangeResourceRecordSetsResponse -> Either ServerError ChangeResourceRecordSetsResponse
validate changeSet =
  if status >= 300
    then Left (toServerError status)
    else Right changeSet

  where
    status = changeSet ^. crrsrsResponseStatus
