module Fission.Authorization.Session.Trans
  ( runSessionT
  , module Fission.Authorization.Session.Trans.Types
  ) where

import           Fission.Prelude

import           Fission.Authorization.Session.Trans.Types
import           Fission.Authorization.Session.Types

runSessionT :: MonadSTM m => Session -> SessionT m a -> m a
runSessionT session action = do
  sessionVar <- atomicallyM $ newTVar session
  runReaderT (unSessionT action) sessionVar
