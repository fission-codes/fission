module Fission.Config
  ( Config (..)
  -- Fields
  , IPFSPath (..)
  , ipfsPath
  , DBPath (..)
  , dbPath
  , DBPool (..)
  , dbPool
  , Host (..)
  , host
  , HerokuID (..)
  , herokuID
  , HerokuPassword (..)
  , herokuPassword
  -- Helpers
  , SeldaPool
  , base
  , logFunc
  , minLogLevel
  ) where

import RIO

import Control.Lens (makeLenses)
import System.Envy

import Data.Has
import Data.Pool
import Database.Selda.Backend

import qualified Fission.Log            as Log
import qualified Fission.Web.Config     as Web
import qualified Fission.Storage.SQLite as DB
import qualified Fission.IPFS.Path      as IPFS

import qualified Fission.Storage.Types

newtype HerokuID = HerokuID { getHerokuID :: ByteString }
  deriving (Show, IsString)

newtype HerokuPassword = HerokuPassword { getHerokuPassword :: ByteString }
  deriving (Show, IsString)

data Config = Config
  { _logFunc        :: !LogFunc
  , _minLogLevel    :: !Log.MinLogLevel
  , _ipfsPath       :: !IPFSPath
  , _host           :: !Host
  , _dbPath         :: !DBPath
  , _dbPool         :: !DBPool
  , _herokuID       :: !HerokuID
  , _herokuPassword :: !HerokuPassword
  }

makeLenses ''Config

instance Has IPFS.Path Config where
  hasLens = ipfsPath

instance Has Log.MinLogLevel Config where
  hasLens = minLogLevel

instance Has DBPath Config where
  hasLens = dbPath

instance Has DBPool Config where
  hasLens = dbPool

instance Has HerokuID Config where
  hasLens = herokuID

instance Has HerokuPassword Config where
  hasLens = herokuPassword

instance Has Host Config where
  hasLens = host

instance HasLogFunc Config where
  logFuncL = logFunc

-- FIXME with Envy
base :: HerokuID -> HerokuPassword -> DBPool -> Config
base hkuID hkuPass pool = Config
    { _logFunc        = mkLogFunc Log.simple
    , _minLogLevel    = Log.MinLogLevel LevelDebug
    , _ipfsPath       = IPFSPath "/usr/local/bin/ipfs"
    , _host           = Host "localhost:3000"
    , _dbPath         = DBPath "ipfs-api.sqlite"
    , _dbPool         = pool
    , _herokuID       = hkuID
    , _herokuPassword = hkuPass
    }
