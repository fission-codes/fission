module Fission.Storage.PostgreSQL
  ( setupTable
  , connPool
  , makeTable
  ) where

import RIO
import RIO.Time

import SuperRecord
import Data.Pool

import Database.Selda
import Database.Selda.PostgreSQL

import Fission.Internal.Constraint
import Fission.Internal.Orphanage.Tuple ()

import qualified Fission.Config        as Config
import qualified Fission.Storage.Table as Table
import qualified Fission.Storage.Types as DB

setupTable :: MonadRIO (Rec cfg) m
           => HasLogFunc (Rec cfg)
           => Has "pgConnectInfo" cfg PGConnectInfo
           => Table b
           -> TableName
           -> m ()
setupTable tbl tblName = do
  pgInfo <- asksR #pgConnectInfo
  logInfo $ "Creating table `" <> displayShow tblName <> "` in DB"
  liftIO . withPostgreSQL pgInfo $ createTable tbl

connPool :: MonadRIO   cfg m
         => HasLogFunc cfg
         => Int
         -> Int
         -> NominalDiffTime
         -> PGConnectInfo
         -> m DB.Pool
connPool stripeCount connsPerStripe connTTL pgInfo@(PGConnectInfo {..}) = do
  logDebug $ "Establishing DB pool for " <> displayShow pgDatabase

  rawPool <- liftIO $ createPool (pgOpen pgInfo) seldaClose stripeCount connTTL connsPerStripe
  logDebug $ "DB pool stats: " <> displayShow rawPool

  return $ DB.Pool rawPool

-- NOTE HLint can't handle BlockArguments _yet_
makeTable :: PGConnectInfo -> Table t -> Table.Name t -> IO ()
makeTable pgInfo' tbl tblName = runSimpleApp do
  pool   <- connPool 1 1 1 pgInfo'
  logger <- view logFuncL
  runRIO (logger, pool, pgInfo') . setupTable tbl $ Table.name tblName
