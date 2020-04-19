module Fission.Web.Client.Auth.Class (MonadWebAuth (..)) where

import           Fission.Prelude

class Monad m => MonadWebAuth m auth where
  getAuth :: m auth
