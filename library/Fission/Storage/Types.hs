module Fission.Storage.Types
  (-- Pool (..)
  -- SeldaPool
  Pool
  ) where

import RIO

import qualified Data.Pool                 as Database
import           Database.Selda.Backend    (SeldaConnection)
import           Database.Selda.PostgreSQL

type SeldaPool = Database.Pool (SeldaConnection PG)
type Pool = Database.Pool (SeldaConnection PG)

-- new Pool = Pool { getPool :: SeldaPool }
  -- deriving Show
