module Fission.Storage.PostgreSQL
  ( setupTable
  , connPool
  , makeTable
  ) where

import RIO

import Data.Has
import Data.Pool

import Database.Selda
import Database.Selda.PostgreSQL

import Fission.Internal.Constraint
import Fission.Internal.Orphanage.Tuple ()

import qualified Fission.Config        as Config
import qualified Fission.Storage.Table as Table
import qualified Fission.Storage.Types as DB

setupTable :: MonadRIO cfg m
           => HasLogFunc cfg
           => Has DB.PGInfo cfg
           => Table b
           -> TableName
           -> m ()
setupTable tbl tblName = do
  pgInfo <- DB.getPGInfo <$> Config.get
  logInfo $ "Creating table `" <> displayShow tblName <> "` in DB"
  liftIO . withPostgreSQL pgInfo $ createTable tbl

-- TODO make configurable
connPool :: HasLogFunc cfg => DB.PGInfo -> RIO cfg DB.Pool
connPool pgInfo = do
  -- logDebug $ "Establishing DB pool for " <> displayShow path

  rawPool <- liftIO $ createPool (pgOpen $ DB.getPGInfo pgInfo) seldaClose 4 2 10 -- config these
  logDebug $ "DB pool stats: " <> displayShow rawPool

  return $ DB.Pool rawPool

-- HLint can't handle BlockArguments _yet_
makeTable :: DB.PGInfo -> Table t -> Table.Name t -> IO ()
makeTable pgInfo' tbl tblName = runSimpleApp do
  pool   <- connPool pgInfo'
  logger <- view logFuncL
  runRIO (logger, pool, pgInfo') . setupTable tbl $ Table.name tblName
