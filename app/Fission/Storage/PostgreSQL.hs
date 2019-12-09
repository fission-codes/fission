module Fission.Storage.PostgreSQL
  ( setupTable
  , connPool
  , makeTable
  ) where

import qualified RIO.Text as Text

-- Database

import           Data.Pool
import qualified Database.Persist.Postgresql as Postgres
import           Database.Persist.SQL (IsSqlBackend)
import qualified Database.PostgreSQL.Simple as Postgres

-- Fission

import           Fission.Prelude
import           Fission.Storage.PostgresQL.Types
import qualified Fission.Storage.Types as DB
import           Fission.Internal.Orphanage.Tuple ()


connPool
  :: ( MonadRIO cfg m
     , HasLogFunc cfg
     , IsSqlBackend backend
     )
  => Int
  -> Int
  -> NominalDiffTime
  -> ConnectionInfo
  -> m (DB.Pool backend)
connPool stripeCount connsPerStripe connTTL connectionInfo = do
  logDebug ("Establishing DB pool for " <> displayShow pgDatabase)

  -- Functions
  let connectionCloser = Postgres.close
  let loggingFunc _ _ _ _ = return () -- TODO

  -- Create database pool
  rawPool <-
    connectionInfo
      |> simpleConnectionInfo
      |> Postgres.connect
      |> bind (Postgres.openSimpleConn loggingFunc)
      |> (\connectionCreator ->
        createPool
          connectionCreator
          connectionCloser
          stripeCount
          connTTL
          connsPerStripe
      )
      |> liftIO

  -- Log database pool statistics
  logDebug ("DB pool stats: " <> displayShow rawPool)

  return (DB.Pool rawPool)



-- ㊙️


simpleConnectionInfo :: ConnectionInfo -> Postgres.ConnectInfo
simpleConnectionInfo (ConnectionInfo {..}) = ConnectInfo
  { connectDatabase = Text.unpack database
  , connectHost = Text.unpack host
  , connectPassword = map Text.unpack password
  , connectPort = port
  , connectUser = map Text.unpack user
  }
