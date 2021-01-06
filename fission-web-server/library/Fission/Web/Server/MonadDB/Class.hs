module Fission.Web.Server.MonadDB.Class (MonadDB (..)) where

import           RIO

class MonadIO m => MonadDB transaction m | m -> transaction where
  runDB :: transaction a -> m a
