module Fission.Storage.Database.Class where

import qualified Database.Persist.Class as Persist
import           Fission.Prelude
import           Fission.Storage.Database.Types


class (Monad m, Persist.PersistField a) => MonadDatabase m a where
  execute :: Transaction m a -> m a
