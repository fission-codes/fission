module Fission.Web.Client.Auth
  ( withAuth
  , module Fission.Web.Client.Auth.Class
  ) where

import           Fission.Prelude
import           Fission.Web.Client.Auth.Class

withAuth :: HasWebAuth m => m auth -> (auth -> m a) -> m a
withAuth auth req = req =<< auth
