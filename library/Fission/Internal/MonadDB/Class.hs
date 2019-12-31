module Fission.Internal.MonadDB.Class (MonadDB (..)) where

import RIO

import Fission.Internal.MonadDB.Types

class MonadIO m => MonadDB m where
  runDB :: Transaction m a -> m a
