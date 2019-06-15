module Fission
  ( Fission
  , mkConfig
  , fromConfig
  , simply
  ) where

import RIO

import Data.Has

import           Fission.Types
import qualified Fission.IPFS.Types            as IPFS
import qualified Fission.Log                   as Log
import qualified Fission.Platform.Heroku.Types as Heroku
import qualified Fission.Storage.Types         as DB
import qualified Fission.Web.Types             as Web

-- FIXME with Envy
mkConfig :: Heroku.ID -> Heroku.Password -> DB.Pool -> Config
mkConfig hkuID hkuPass pool = Config
    { _logFunc        = mkLogFunc Log.simple
    , _minLogLevel    = Log.MinLogLevel LevelDebug
    , _ipfsPath       = IPFS.Path "/usr/local/bin/ipfs"
    , _host           = Web.Host "localhost:3000"
    , _dbPath         = DB.Path "ipfs-api.sqlite"
    , _dbPool         = pool
    , _herokuID       = hkuID
    , _herokuPassword = hkuPass
    }

fromConfig :: (MonadReader cfg m, Has a cfg) => m a
fromConfig = view hasLens

simply :: RIO LogFunc a -> IO a
simply = runRIO (mkLogFunc Log.simple)
