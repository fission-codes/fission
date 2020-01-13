module Fission.User.Authorizer.Class (Authorizer (..)) where

import           Servant
import           Fission.Prelude

import Fission.Models
import Database.Esqueleto

-- | Authorization for the Heroku Partner API
class Monad m => Authorizer m where
  -- | Verify that the sender is the Heroku Partner web service
  verify :: m (BasicAuthCheck (Entity User))
