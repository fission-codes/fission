-- | Purge method, for NGINX cache busting

module Fission.Web.Server.Internal.NGINX.Purge.Types (PURGE (..)) where

import           Network.HTTP.Req

data PURGE = PURGE

instance HttpMethod PURGE where
  type AllowsBody _ = 'NoBody
  httpMethodName _pxy = "PURGE"
