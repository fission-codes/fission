module Fission.Web.Client.Class (MonadWebClient (..)) where

import           Servant.Client
import           Fission.Prelude

class Monad m => MonadWebClient m where
  sendRequest :: ClientM a -> m (Either ClientError a)
