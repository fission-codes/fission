module Fission.Internal.MonadDB.Types (Transaction) where

import Database.Persist.Sql
import RIO

type Transaction m a = ReaderT SqlBackend m a
