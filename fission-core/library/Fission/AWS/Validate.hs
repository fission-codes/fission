module Fission.AWS.Validate (validate) where

import           Fission.Prelude
import           Servant

import           Fission.StatusCode
import           Fission.Web.Error


validate :: HasStatusCode a => a -> Either ServerError a
validate res = if status >= 300 then Left (toServerError status) else Right res
  where status = unStatusCode $ getStatusCode res
