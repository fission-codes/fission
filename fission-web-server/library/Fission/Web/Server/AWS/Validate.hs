module Fission.Web.Server.AWS.Validate (validate) where

import           Servant

import           Fission.Prelude

import           Fission.Web.Server.Error
import           Fission.Web.Server.StatusCode

validate :: Has StatusCode a => a -> Either ServerError a
validate res =
  if status >= 300
    then Left (toServerError status)
    else Right res

  where
    status = unStatusCode $ getter res
