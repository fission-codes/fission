module Fission.CLI.GitHub.Class (MonadGitHub (..)) where

import           Fission.Prelude
import           Servant.Client

class Monad m => MonadGitHub m where
  sendRequest :: ClientM a -> m (Either ClientError a)
