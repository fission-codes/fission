module Fission.User.Authorizer.Class (Authorizer (..)) where

import           Servant
import           Fission.Prelude

import           Fission.Models
import           Database.Esqueleto

-- | User authorization
class Monad m => Authorizer m where
  -- | Verify that the user is registered
  verify :: m (BasicAuthCheck (Entity User))
