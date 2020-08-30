module Fission.Web.Redirect (redirect) where

import Fission.Prelude
import Servant

-- Servant treats all non-200s as "errors" so we redirect by "throwing" a 301
redirect :: 
     MonadThrow m
  => ByteString
  -> m ()
redirect location = throwM err301 { errHeaders = [("Location", location)] }
