module Fission.User.DID.NameService.Class (MonadNameService (..)) where

import           Fission.Prelude

import           Fission.User.DID.Types
import           Fission.User.Username.Types

class Monad m => MonadNameService m where
  getByUsername :: Username -> m (Either (NotFound DID) DID)
