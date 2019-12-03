module Fission.Storage.Class

import qualified Database.Persist as Persist
import           Fission.Storage.Types


class (Monad m, Persist.SqlPersistM m) => MonadDatabase m where
  execute :: Query a -> m a
