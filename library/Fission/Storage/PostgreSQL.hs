module Fission.Storage.PostgreSQL
  (-- setupTable
  connPool
  -- , makeTable
  ) where

import RIO
import RIO.Time

import Data.Pool
import SuperRecord

import Database.Selda
import Database.Selda.PostgreSQL

import Fission.Internal.Constraint
import Fission.Internal.Orphanage.Tuple ()

import qualified Fission.Config        as Config
import qualified Fission.Storage.Table as Table
import qualified Fission.Storage.Types as DB

-- setupTable :: MonadRIO (Rec cfg) m
--            => HasLogFunc (Rec cfg)
--            => Has "pgConnectInfo" cfg PGConnectInfo
--            => Table b
--            -> TableName
--            -> m ()
-- setupTable tbl tblName = do
--   pgInfo <- asksR #pgConnectInfo
--   logInfo $ "Creating table `" <> displayShow tblName <> "` in DB"
--   liftIO . withPostgreSQL pgInfo $ createTable tbl

connPool :: MonadRIO   (Rec cfg) m
         => HasLogFunc (Rec cfg)
         => Int
         -> Int
         -> NominalDiffTime
         -> PGConnectInfo
         -> m DB.Pool
connPool stripeCount connsPerStripe connTTL pgInfo@(PGConnectInfo {..}) = do
  rawPool <- liftIO $ createPool (pgOpen pgInfo) seldaClose stripeCount connTTL connsPerStripe
  return $ DB.Pool rawPool

-- makeTable :: PGConnectInfo -> Table t -> Table.Name t -> IO ()
-- makeTable pgInfo' tbl tblName = runRIO (SuperRecord.& rnil) do
--   pool   <- connPool 1 1 1 pgInfo'
--   logger <- view logFuncL
--   runRIO (logger, pool, pgInfo') . setupTable tbl $ Table.name tblName
