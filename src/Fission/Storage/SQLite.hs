module Fission.Storage.SQLite
  ( setupTable
  , connPool
  , makeTable
  ) where

import RIO

import Data.Has
import Data.Pool

import Database.Selda
import Database.Selda.SQLite
import Database.Selda.Backend

import System.Envy

import           Fission.Internal.Constraint
import           Fission.Internal.Orphanage ()
import qualified Fission.Storage.Table       as Table
import qualified Fission.Log                 as Log
import qualified Fission.Storage.Types       as DB

setupTable :: MonadRIO cfg m
           => HasLogFunc cfg
           => Has DB.Path cfg
           => Table b
           -> TableName
           -> m ()
setupTable tbl tblName = do
  DB.Path db <- fromCfg
  logInfo $ "Creating table `" <> displayShow tblName <> "` in DB " <> displayShow db
  liftIO . withSQLite db $ createTable tbl

-- TODO make configurable
connPool :: HasLogFunc cfg => DB.Path -> RIO cfg DB.Pool
connPool (DB.Path {getPath = path}) = do
  logInfo $ "Establishing database connection for " <> displayShow path

  rawPool <- liftIO $ createPool (sqliteOpen path) seldaClose 4 2 10 -- config these
  logInfo $ "DB pool stats: " <> displayShow rawPool

  return $ DB.Pool rawPool

makeTable :: DB.Path -> Table t -> Table.Name t -> IO ()
makeTable dbPath' tbl tblName = runRIO logger do
  pool <- connPool dbPath'
  runRIO (logger, DB.Pool pool, dbPath') $ setupTable tbl (Table.name tblName)
  where
    logger  = mkLogFunc Log.simple
