-- | Configuration types
module Fission.Config.Types
  ( Config (..)
  , processCtx
  , logFunc
  , ipfsPath
  , ipfsTimeout
  , host
  , pgInfo
  , dbPool
  , herokuID
  , herokuPassword
  ) where

import RIO
import RIO.List (intercalate)
import RIO.Process (ProcessContext, HasProcessContext (..))

import           Control.Lens (makeLenses)
import           Data.Has
import qualified Network.HTTP.Client as HTTP

import           Fission.Web.Types
import qualified Fission.IPFS.Types            as IPFS
import qualified Fission.Storage.Types         as DB
import qualified Fission.Platform.Heroku.Types as Heroku

-- | The top level 'Fission' application 'RIO' configuration
data Config = Config
  { _processCtx     :: !ProcessContext
  , _logFunc        :: !LogFunc
  , _httpManager    :: !HTTP.Manager
  , _ipfsPath       :: !IPFS.BinPath
  , _ipfsURL        :: !IPFS.URL
  , _ipfsTimeout    :: !IPFS.Timeout
  , _host           :: !Host
  , _pgInfo         :: !DB.PGInfo
  , _dbPool         :: !DB.Pool
  , _herokuID       :: !Heroku.ID
  , _herokuPassword :: !Heroku.Password
  }

makeLenses ''Config

instance Show Config where
  show Config {..} = intercalate "\n"
    [ "Config {"
    , "  _processCtx     = **SOME PROC CONTEXT**"
    , "  _logFunc        = **SOME LOG FUNCTION**"
    , "  _httpManager    = **SOME HTTP MANAGER**"
    , "  _ipfsPath       = " <> show _ipfsPath
    , "  _ipfsURL        = " <> show _ipfsURL
    , "  _ipfsTimeout    = " <> show _ipfsTimeout
    , "  _host           = " <> show _host
    , "  _pgInfo         = " <> show _pgInfo
    , "  _dbPool         = " <> show _dbPool
    , "  _herokuID       = " <> show _herokuID
    , "  _herokuPassword = " <> show _herokuPassword
    , "}"
    ]

instance HasProcessContext Config where
  processContextL = processCtx

instance HasLogFunc Config where
  logFuncL = logFunc

instance Has HTTP.Manager Config where
  hasLens = httpManager

instance Has IPFS.BinPath Config where
  hasLens = ipfsPath

instance Has IPFS.URL Config where
  hasLens = ipfsURL

instance Has IPFS.Timeout Config where
  hasLens = ipfsTimeout

instance Has DB.PGInfo Config where
  hasLens = pgInfo

instance Has DB.Pool Config where
  hasLens = dbPool

instance Has Heroku.ID Config where
  hasLens = herokuID

instance Has Heroku.Password Config where
  hasLens = herokuPassword

instance Has Host Config where
  hasLens = host
