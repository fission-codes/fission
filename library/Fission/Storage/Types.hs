module Fission.Storage.Types
  ( Pool (..)
  , PGInfo (..)
  , SeldaPool
  ) where

import RIO

import qualified Data.ByteString.Char8     as BS
import qualified Data.Pool                 as Database
import           Database.Selda.Backend    (SeldaConnection)
import           Database.Selda.PostgreSQL

type SeldaPool = Database.Pool (SeldaConnection PG)

newtype Pool = Pool { getPool :: SeldaPool }
  deriving Show

newtype PGInfo = PGInfo { getPGInfo :: PGConnectInfo }

instance Show PGInfo where
  show = BS.unpack . pgConnString . getPGInfo
