-- | Configuration types
module Fission.Config.Types
  ( Config (..)
  , processCtx
  , logFunc
  , ipfsPath
  , ipfsTimeout
  , host
  , pgConnectInfo
  , dbPool
  , herokuID
  , herokuPassword
  ) where

import           RIO
import           RIO.List (intercalate)
import           RIO.Process (ProcessContext, HasProcessContext (..))

import           Control.Lens (makeLenses)
import           Data.Has
import           Database.Selda.PostgreSQL
import qualified Network.HTTP.Client as HTTP

import           Fission.Web.Types
import qualified Fission.IPFS.Types            as IPFS
import qualified Fission.Storage.Types         as DB
import qualified Fission.Platform.Heroku.Types as Heroku
import qualified Network.AWS.Auth              as AWS
import qualified Fission.AWS.Types              as AWS
import           Fission.Internal.Orphanage.PGConnectInfo ()

-- | The top level 'Fission' application 'RIO' configuration
data Config = Config
  { _processCtx     :: !ProcessContext
  , _logFunc        :: !LogFunc
  , _httpManager    :: !HTTP.Manager
  , _ipfsPath       :: !IPFS.BinPath
  , _ipfsURL        :: !IPFS.URL
  , _ipfsTimeout    :: !IPFS.Timeout
  , _host           :: !Host
  , _pgConnectInfo  :: !PGConnectInfo
  , _dbPool         :: !DB.Pool
  , _herokuID       :: !Heroku.ID
  , _herokuPassword :: !Heroku.Password
  , _awsAccessKey   :: !AWS.AccessKey
  , _awsSecretKey   :: !AWS.SecretKey
  , _awsZoneId      :: !AWS.ZoneId
  , _awsDomain      :: !AWS.Domain
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
    , "  _pgConnectInfo  = " <> show _pgConnectInfo
    , "  _dbPool         = " <> show _dbPool
    , "  _herokuID       = " <> show _herokuID
    , "  _herokuPassword = " <> show _herokuPassword
    , "  _awsAccessKey   = " <> show _awsAccessKey
    , "  _awsSecretKey   = HIDDEN"
    , "  _awsZoneId      = " <> show _awsZoneId
    , "  _awsDomain      = " <> show _awsDomain
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

instance Has PGConnectInfo Config where
  hasLens = pgConnectInfo

instance Has DB.Pool Config where
  hasLens = dbPool

instance Has Heroku.ID Config where
  hasLens = herokuID

instance Has Heroku.Password Config where
  hasLens = herokuPassword

instance Has AWS.AccessKey Config where
  hasLens = awsAccessKey

instance Has AWS.SecretKey Config where
  hasLens = awsSecretKey

instance Has AWS.ZoneId Config where
  hasLens = awsZoneId

instance Has AWS.Domain Config where
  hasLens = awsDomain

instance Has Host Config where
  hasLens = host
