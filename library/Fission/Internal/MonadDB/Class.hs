{-# LANGUAGE UndecidableInstances #-}

module Fission.Internal.MonadDB.Class (MonadDB (..)) where

import Data.Has
import Data.Pool
import Database.Persist.Sql as Sql

import RIO

import Fission.Internal.MonadDB.Types

class MonadIO m => MonadDB m where
  runDB :: Transaction m a -> m a

instance Has (Pool SqlBackend) cfg => MonadDB (RIO cfg) where
  runDB transaction = do
    pool <- view hasLens
    runSqlPool transaction pool
