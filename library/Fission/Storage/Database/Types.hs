module Fission.Storage.Database.Types
  ( Pool(..)
  , Query
  , Transaction
  ) where

import Database.Esqueleto (SqlBackend)
import qualified Data.Pool as Database
import qualified Database.Esqueleto as Esqueleto
import Fission.Prelude


{-| Newtype for getting the database pool out of the app config.
-}
newtype Pool databaseBackend = Pool
  { getDatabasePool :: Database.Pool databaseBackend }
  deriving Show


{-| Alias for the esqueleto SqlQuery type.
-}
type Query a = Esqueleto.SqlQuery a


{-| Alias for the ReaderT type.
-}
type Transaction m a = (MonadIO m) => ReaderT SqlBackend m a
