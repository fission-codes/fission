module Fission.Types
  ( Fission
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
import qualified Fission.IPFS.Types            as IPFS
import qualified Fission.Log                   as Log
import qualified Fission.Storage.Types         as DB
import qualified Fission.Platform.Heroku.Types as Heroku

-- | Top-level application type
type Fission = RIO Config

data Config = Config
  { _logFunc        :: !LogFunc
  , _minLogLevel    :: !Log.MinLogLevel
  , _ipfsPath       :: !IPFS.Path
  , _host           :: !Host
  , _dbPath         :: !DB.Path
  , _dbPool         :: !DB.Pool
  , _herokuID       :: !Heroku.ID
  , _herokuPassword :: !Heroku.Password
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

instance Has Heroku.ID Config where
  hasLens = herokuID

instance Has Heroku.Password Config where
  hasLens = herokuPassword

instance Has Host Config where
  hasLens = host

instance HasLogFunc Config where
  logFuncL = logFunc
