module Fission.Web.Server.Storage.PostgreSQL
  ( updateDBToLatest
  , withDBPool
  , module Fission.Web.Server.Storage.PostgreSQL.ConnectionInfo.Types
  , module Fission.Web.Server.Storage.PostgreSQL.PoolSize.Types
  ) where

import qualified Data.ByteString.Char8                                      as BS8
import           Data.Pool

import           Database.Esqueleto                                         (SqlBackend,
                                                                             runMigration)
import           Database.Persist.Postgresql                                (withPostgresqlPool)

import           Fission.Prelude

import           Fission.Web.Server.Models
import           Fission.Web.Server.MonadDB.Types

import           Fission.Web.Server.Storage.PostgreSQL.ConnectionInfo.Types
import           Fission.Web.Server.Storage.PostgreSQL.PoolSize.Types

-- | Automagically update the database schema to reflect the latest schema in Models
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
