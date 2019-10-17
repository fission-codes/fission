module Fission.Web.Client.Types (Runner (..), mkRunner) where

import RIO

import Servant.Client

newtype Runner = Runner
  { getRunner :: forall m a. MonadIO m => ClientM a -> m (Either ClientError a) }

mkRunner :: MonadIO m => (a -> IO b) -> (a -> m b)
mkRunner = fmap liftIO
