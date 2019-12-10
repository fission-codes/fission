module Fission.Storage.Database.Class where

import qualified Database.Persist.Class as Persist
import qualified Database.Persist.Sql as Persist

import qualified Fission.Config as Config
import           Fission.Prelude
import           Fission.Storage.Database.Types


execute
  :: ( MonadRIO cfg m
     , MonadUnliftIO m
     , Has (Pool Persist.SqlBackend) cfg
     )
  => Transaction m a
  -> m a
execute transaction = do
  Pool pool <- Config.get
  Persist.runSqlPool transaction pool
