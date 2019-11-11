module Fission.Storage.Types
  ( Pool (..)
  , SeldaPool
  ) where

import RIO

import qualified Data.Pool              as Database
import           Database.Selda.Backend (SeldaConnection)

type SeldaPool be = Database.Pool (SeldaConnection be)

newtype Pool be = Pool { getPool :: SeldaPool be }
  deriving Show
