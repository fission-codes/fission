module Fission.Storage.Types
  ( Pool (..)
  ) where

import Fission.Prelude
import qualified Data.Pool


{-| Alias for the monad from the Persist library.
-}
type Query m a = (MonadIO m, MonadLogger m) => SqlPersistM m a


{-| TODO: Don't know how to describe this yet.
-}
newtype Pool databaseBackend = Pool
  { getPool :: Data.Pool databaseBackend }
  deriving Show
