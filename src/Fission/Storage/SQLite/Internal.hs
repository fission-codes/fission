module Fission.Storage.SQLite.Internal (nt) where

import RIO

import Data.Has
import Data.Pool

import Database.Beam.Sqlite

import qualified Fission.Config        as Config
import           Fission.Log.Types
import qualified Fission.Storage.Types as DB

nt :: (Has Logger cfg, Has DB.Pool cfg) => SqliteM a -> RIO cfg a
nt action = do
  DB.Pool pool  <- Config.get
  Logger logger <- Config.get
  let runLogger = runRIO logger . logDebug . displayShow
  liftIO . withResource pool $ \conn ->
    runReaderT (runSqliteM action) (runLogger, conn)
