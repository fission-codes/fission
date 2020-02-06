module Fission.AWS.Error (validate) where

import RIO

import Servant

import Fission.AWS.Response as AWS
import Fission.Web.Error

validate :: AWS.Response a => a -> Either ServerError a
validate res =
  if code >= 300
    then Left (toServerError code)
    else Right res

  where
    code = status res
