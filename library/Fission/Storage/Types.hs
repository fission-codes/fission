module Fission.Storage.Types
  ( Path (..)
  , Pool (..)
  , SeldaPool
  ) where

import RIO

import qualified Data.Pool              as Database
import           Data.Swagger           (ToSchema)
import           Database.Selda.Backend (SeldaConnection)
import           Database.Selda.SQLite

import System.Envy

type SeldaPool = Database.Pool (SeldaConnection SQLite)

newtype Pool = Pool { getPool :: SeldaPool }
  deriving Show

newtype Path = Path { getPath :: FilePath }
  deriving          ( Show
                    , Generic
                    )
  deriving anyclass ( FromEnv
                    , ToSchema
                    )
  deriving newtype  ( IsString )
