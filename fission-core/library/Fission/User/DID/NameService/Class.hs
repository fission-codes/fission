module Fission.User.DID.NameService.Class (MonadNameService (..)) where

import           Fission.Prelude

import           Fission.Error.NotFound.Types

import           Fission.User.Username.Types
import           Web.DID.Types

class Monad m => MonadNameService m where
  getByUsername :: Username -> m (Either (NotFound DID) DID)
