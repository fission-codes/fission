module Fission.Storage.Types
  ( Pool (..)
  , SeldaPool
  ) where

import qualified Data.Pool              as Database
import           Database.Selda.Backend (SeldaConnection)

import Fission.Prelude

type SeldaPool backend = Database.Pool (SeldaConnection backend)

newtype Pool backend = Pool { getPool :: SeldaPool backend }
  deriving Show
