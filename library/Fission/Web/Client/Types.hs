module Fission.Web.Client.Types (Runner (..)) where

import RIO

import Servant.Client

newtype Runner = Runner
  { getRunner :: forall m a. MonadIO m => ClientM a -> m (Either ClientError a) }
