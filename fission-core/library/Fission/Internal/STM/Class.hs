module Fission.Internal.STM.Class (MonadSTM (..)) where

import           RIO

class Monad m => MonadSTM m where
  atomicallyM :: STM a -> m a
