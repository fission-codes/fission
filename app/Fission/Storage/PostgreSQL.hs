module Fission.Storage.PostgreSQL
  ( setupTable
  , connPool
  , makeTable
  ) where

import Data.Pool
import Database.Selda
import Database.Selda.PostgreSQL

import           Fission.Prelude
import qualified Fission.Storage.Table as Table
import qualified Fission.Storage.Types as DB
import           Fission.Internal.Orphanage.Tuple ()
import           Fission.App (runApp)

setupTable
  :: ( MonadRIO cfg m
     , HasLogFunc cfg
     )
  => PGConnectInfo
  -> Table b
  -> TableName
  -> m ()
setupTable pgInfo tbl tblName = do
  logInfo <| "Creating table `" <> displayShow tblName <> "` in DB"
  liftIO <| withPostgreSQL pgInfo <| createTable tbl

connPool
  :: ( MonadRIO   cfg m
     , HasLogFunc cfg
     )
  => Int
  -> Int
  -> NominalDiffTime
  -> PGConnectInfo
  -> m (DB.Pool PG)
connPool stripeCount connsPerStripe connTTL pgInfo@(PGConnectInfo {..}) = do
  logDebug <| "Establishing DB pool for " <> displayShow pgDatabase

  rawPool <- liftIO <| createPool (pgOpen pgInfo) seldaClose stripeCount connTTL connsPerStripe
  logDebug <| "DB pool stats: " <> displayShow rawPool

  return <| DB.Pool rawPool

-- NOTE HLint can't handle BlockArguments _yet_
makeTable :: PGConnectInfo -> Table t -> Table.Name t -> IO ()
makeTable pgInfo' tbl tblName = runApp do
  pool   <- connPool 1 1 1 pgInfo'
  logger <- view logFuncL
  runRIO (logger, pool, pgInfo') <| setupTable pgInfo' tbl <| Table.name tblName
