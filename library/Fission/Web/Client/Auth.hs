module Fission.Web.Client.Auth
  ( withAuth
  , module Fission.Web.Client.Auth.Class
  ) where

import           Fission.Prelude
import           Fission.Web.Client.Auth.Class

withAuth :: MonadWebAuth m auth => (auth -> a) -> m a
withAuth needsAuth = pure . needsAuth =<< getAuth
