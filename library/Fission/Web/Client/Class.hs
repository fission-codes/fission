module Fission.Web.Client.Class (MonadWebClient (..)) where

import           Fission.Prelude
import           Servant.Client

-- | A monad representing access to a Fission server
class Monad m => MonadWebClient m where
  run :: forall a. ClientM a -> m (Either ClientError a) 
