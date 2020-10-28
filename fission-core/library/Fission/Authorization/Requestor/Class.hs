module Fission.Authorization.Requestor.Class where

import           Fission.Prelude

import           Fission.Authorization.Heroku.Types
import           Fission.User.DID.Types

class MonadRequestor m where
  getRequestor :: m (Either Heroku DID)
