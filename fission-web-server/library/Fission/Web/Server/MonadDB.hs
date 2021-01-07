module Fission.Web.Server.MonadDB
  ( module Fission.Web.Server.MonadDB.Types
  , module Fission.Web.Server.MonadDB.Class
  , runDBNow
  , getInner
  , ensureEntity
  , ensureEntityM
  ) where

import           Control.Monad.Logger
import           Control.Monad.Time

import           Database.Esqueleto

import           RIO                              hiding (logError)
import           RIO.Time

import           Fission.Web.Server.MonadDB.Class
import           Fission.Web.Server.MonadDB.Types

runDBNow :: (MonadTime m, MonadDB trans m) => (UTCTime -> trans a) -> m a
runDBNow timeTransaction = do
  now <- currentTime
  runDB (timeTransaction now)

ensureEntity :: (Exception err, MonadLogger m, MonadThrow m) => err -> Maybe a -> m a
ensureEntity err = \case
  Nothing -> throwM err
  Just x  -> return x

ensureEntityM :: (Exception err, MonadLogger m, MonadThrow m) => err -> m (Maybe a) -> m a
ensureEntityM err maybeEntity = ensureEntity err =<< maybeEntity

getInner :: (model -> a) -> Entity model -> a
getInner accessor (Entity _ model) = accessor model
