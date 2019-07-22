module Fission.Storage.Types
  ( Path (..)
  , Pool (..)
  ) where

import RIO

import qualified Data.Pool              as Database
import           Data.Swagger           (ToSchema)
import           Database.SQLite.Simple as SQLite
import           System.Envy

type SQLPool = Database.Pool SQLite.Connection

newtype Pool = Pool { getPool :: SQLPool }
  deriving Show

newtype Path = Path { getPath :: FilePath }
  deriving          ( Show
                    , Generic
                    )
  deriving anyclass ( ToSchema )
  deriving newtype  ( IsString )

instance FromEnv Path where
  fromEnv = Path <$> env "DB_PATH"
