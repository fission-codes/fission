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

import Fission.Internal.Constraint
import Fission.Internal.Orphanage ()

import qualified Fission.Config        as Config
import qualified Fission.Storage.Table as Table
import qualified Fission.Storage.Types as DB

setupTable :: MonadRIO cfg m
           => HasLogFunc cfg
           => Has DB.Path cfg
           => Table b
           -> TableName
           -> m ()
setupTable tbl tblName = do
  DB.Path db <- Config.get
  logInfo $ "Creating table `" <> displayShow tblName <> "` in DB " <> displayShow db
  liftIO . withSQLite db $ createTable tbl

-- TODO make configurable
connPool :: HasLogFunc cfg => DB.Path -> RIO cfg DB.Pool
connPool (DB.Path {getPath = path}) = do
  logDebug $ "Establishing DB pool for " <> displayShow path

  rawPool <- liftIO $ createPool (sqliteOpen path) seldaClose 4 2 10 -- config these
  logDebug $ "DB pool stats: " <> displayShow rawPool

  return $ DB.Pool rawPool

-- HLint can't handle BlockArguments _yet_
makeTable :: DB.Path -> Table t -> Table.Name t -> IO ()
makeTable dbPath' tbl tblName = runSimpleApp do
  pool   <- connPool dbPath'
  logger <- view logFuncL
  runRIO (logger, pool, dbPath') $ setupTable tbl (Table.name tblName)
