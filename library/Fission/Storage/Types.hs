module Fission.Storage.Types
  ( Pool (..)
  , Query
  ) where

import Fission.Prelude
import qualified Database.Persist.Sql as Persist
import qualified Data.Pool as Database


{-| Alias for the monad from the Persist library.
-}
type Query m a = Persist.SqlPersistT m a


{-| Newtype for getting the database pool out of the app config.
-}
newtype Pool databaseBackend = Pool
  { getDatabasePool :: Database.Pool databaseBackend }
  deriving Show
