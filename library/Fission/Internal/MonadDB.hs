module Fission.Internal.MonadDB
  ( module Fission.Internal.MonadDB.Types
  , module Fission.Internal.MonadDB.Class
  , runDBNow
  , getInner
  , ensureOne
  , ensureOneId
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
