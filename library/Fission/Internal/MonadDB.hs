module Fission.Internal.MonadDB
  ( module Fission.Internal.MonadDB.Types
  , module Fission.Internal.MonadDB.Class
  , runDBNow
  , getInner
  , ensureOne
  , ensureOneId
  , ensureEntity
  , ensureEntityM
  ) where

import Control.Monad.Logger
import Control.Monad.Time

import Database.Esqueleto

import RIO.Time
import RIO hiding (logError)

import Fission.Internal.MonadDB.Types
import Fission.Internal.MonadDB.Class

runDBNow :: (MonadTime m, MonadDB m) => (UTCTime -> Transaction m a) -> m a
runDBNow timeTransaction = do
  now <- currentTime
  runDB (timeTransaction now)

ensureEntity :: (Exception err, MonadLogger m, MonadThrow m) => err -> Maybe a -> m a
ensureEntity err = \case
  Nothing -> throwM err
  Just x  -> return x

ensureEntityM :: (Exception err, MonadLogger m, MonadThrow m) => err -> m (Maybe a) -> m a
ensureEntityM err maybeEntity = ensureEntity err =<< maybeEntity

ensureOneId :: (Exception err, MonadLogger m, MonadThrow m) => err -> [Entity a] -> m (Key a)
ensureOneId err entities = pure . entityKey =<< ensureOne err entities

ensureOne :: (Exception err, MonadLogger m, MonadThrow m) => err -> [a] -> m a
ensureOne err = \case
  []          -> throwM err
  [x]         -> return x
  (x : _ : _) -> do
    logErrorN ("Too many database returns" :: Text)
    return x

getInner :: (model -> a) -> Entity model -> a
getInner accessor (Entity _ model) = accessor model
