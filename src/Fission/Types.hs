module Fission.Types
  ( Fission
  , HerokuID (..)
  , HerokuPassword (..)
  , Config (..)
  , logFunc
  , minLogLevel
  , ipfsPath
  , host
  , dbPath
  , dbPool
  , herokuID
  , herokuPassword
  ) where

import RIO

import Control.Lens (makeLenses)
import Data.Has

import           Fission.Web.Types
import qualified Fission.IPFS.Types    as IPFS
import qualified Fission.Log           as Log
import qualified Fission.Storage.Types as DB

-- | Top-level application type
type Fission = RIO Config

newtype HerokuID = HerokuID { getHerokuID :: ByteString }
  deriving (Show, IsString)

newtype HerokuPassword = HerokuPassword { getHerokuPassword :: ByteString }
  deriving (Show, IsString)

data Config = Config
  { _logFunc        :: !LogFunc
  , _minLogLevel    :: !Log.MinLogLevel
  , _ipfsPath       :: !IPFS.Path
  , _host           :: !Host
  , _dbPath         :: !DB.Path
  , _dbPool         :: !DB.Pool
  , _herokuID       :: !HerokuID
  , _herokuPassword :: !HerokuPassword
  }

makeLenses ''Config

instance Has IPFS.Path Config where
  hasLens = ipfsPath

instance Has Log.MinLogLevel Config where
  hasLens = minLogLevel

instance Has DB.Path Config where
  hasLens = dbPath

instance Has DB.Pool Config where
  hasLens = dbPool

instance Has HerokuID Config where
  hasLens = herokuID

instance Has HerokuPassword Config where
  hasLens = herokuPassword

instance Has Host Config where
  hasLens = host

instance HasLogFunc Config where
  logFuncL = logFunc
