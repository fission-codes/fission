module Fission.Platform.Heroku.Authorizer.Class (Authorizer (..)) where

import           Servant
import           Fission.Prelude

-- | Authorization for the Heroku Partner API
class Monad m => Authorizer m where
  -- | Verify that the sender is the Heroku Partner web service
  verify :: m (BasicAuthCheck ByteString)
