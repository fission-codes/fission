module Fission.Web.Server.HTTP.Cache.Class (MonadHTTPCache (..)) where

import           Servant.Client

import           Fission.Prelude

import           Fission.URL
import           Fission.Web.Server.HTTP.Cache.Error

class Monad m => MonadHTTPCache m where
  -- | Purge all pages under a URL
  purgeURL :: URL -> m (Either ResponseError ())
