module Fission.Storage.Database.Types
  ( Pool (..)
  , Query
  , Transaction
  ) where

import Fission.Prelude
import qualified Database.Esqueleto as Esqueleto
import qualified Data.Pool as Database


{-| Newtype for getting the database pool out of the app config.
-}
newtype Pool databaseBackend = Pool
  { getDatabasePool :: Database.Pool databaseBackend }
  deriving Show


{-| Alias for the esqueleto SqlQuery type.
-}
type Query a = Esqueleto.SqlQuery a


{-| Alias for the monad from the Persist library.
-}
type Transaction m a = Esqueleto.SqlReadT m a
