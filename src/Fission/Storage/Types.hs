module Fission.Storage.Types
  ( Path (..)
  , Pool (..)
  , SeldaPool
  ) where

import RIO

import qualified Data.Pool              as Database
import           Database.Selda.Backend (SeldaConnection)
import           System.Envy

type SeldaPool = Database.Pool SeldaConnection

newtype Pool = Pool { getPool :: SeldaPool }

newtype Path = Path { getPath :: FilePath }
  deriving         Show
  deriving newtype IsString

instance FromEnv Path where
  fromEnv = Path <$> env "DB_PATH"
