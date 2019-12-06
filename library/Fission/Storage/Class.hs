module Fission.Storage.Class where

import qualified Database.Persist.Class as Persist
import           Fission.Prelude
import           Fission.Storage.Types


class (Monad m, Persist.PersistField a) => MonadDatabase m a where
  execute :: Query m a -> m a
