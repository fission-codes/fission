module Fission.Web.Client.Class (MonadWebClient (..)) where

import           Fission.Prelude
import           Servant.Client

class Monad m => MonadWebClient m where
  sendRequest :: ClientM a -> m (Either ClientError a)
