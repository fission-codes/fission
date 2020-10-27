module Fission.User.Namespace.Class (MonadUserNamespace (..)) where

import           Fission.Prelude

import           Fission.URL.Types as URL

class Monad m => MonadUserNamespace m where
  getUserNamespace :: m DomainName
