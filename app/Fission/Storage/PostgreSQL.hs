module Fission.Storage.PostgreSQL
  ( updateDBToLatest
  , withDBPool
  , module Fission.Storage.PostgreSQL.ConnectionInfo.Types
  , module Fission.Storage.PostgreSQL.PoolSize.Types
  ) where

import qualified Data.ByteString.Char8 as BS8
import           Data.Pool

import           Database.Esqueleto (SqlBackend, runMigration)
import           Database.Persist.Postgresql (withPostgresqlPool)

import           Fission.Prelude
import           Fission.Storage.PostgreSQL.ConnectionInfo.Types
import           Fission.Storage.PostgreSQL.PoolSize.Types

import           Fission.Models

-- | Automagically update the database schema to reflect the latest schema in Fission.Models
--
--   Example:
--
--   > runOne (runDB updateDBToLatest)
updateDBToLatest :: MonadIO m => Transaction m ()
updateDBToLatest = runMigration migrateAll

withDBPool ::
  MonadIO m
  => LogFunc
  -> ConnectionInfo
  -> PoolSize
  -> (Pool SqlBackend -> RIO LogFunc a)
  -> m a
withDBPool logger connInfo (PoolSize connCount) actions =
  actions
    |> withPostgresqlPool (connInfo |> show |> BS8.pack) (fromIntegral connCount)
    |> runRIO logger
    |> liftIO
