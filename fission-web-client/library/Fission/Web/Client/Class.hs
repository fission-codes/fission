module Fission.Web.Client.Class (MonadWebClient (..)) where

import           Servant.API
import           Servant.Client
import qualified Servant.Client.Streaming as Streaming

import           Fission.Prelude

class Monad m => MonadWebClient m where
  sendRequest :: ClientM a -> m (Either ClientError a)
  streamWith  :: Streaming.ClientM (SourceIO a) -> (Either ClientError (SourceIO a) -> m b) -> m b
