-- | Configuration types
module Fission.Config.Types (Config (..)) where

import           Database.Selda.PostgreSQL
import qualified Network.HTTP.Client as HTTP
import           RIO.List (intercalate)

import           Fission.Prelude
import           Fission.Web.Types
import qualified Fission.IPFS.Types            as IPFS
import qualified Fission.Storage.Types         as DB
import qualified Fission.Platform.Heroku.Types as Heroku
import qualified Network.AWS.Auth              as AWS
import qualified Fission.AWS.Types              as AWS
import           Fission.Internal.Orphanage.PGConnectInfo ()

-- | The top level 'Fission' application 'RIO' configuration
data Config = Config
  { processCtx     :: !ProcessContext
  , logFunc        :: !LogFunc
  , httpManager    :: !HTTP.Manager
  , ipfsPath       :: !IPFS.BinPath
  , ipfsURL        :: !IPFS.URL
  , ipfsTimeout    :: !IPFS.Timeout
  , host           :: !Host
  , pgConnectInfo  :: !PGConnectInfo
  , dbPool         :: !DB.Pool
  , herokuID       :: !Heroku.ID
  , herokuPassword :: !Heroku.Password
  , awsAccessKey   :: !AWS.AccessKey
  , awsSecretKey   :: !AWS.SecretKey
  , awsZoneID      :: !AWS.ZoneID
  , awsDomainName  :: !AWS.DomainName
  } deriving Generic

instance Show Config where
  show Config {..} = intercalate "\n"
    [ "Config {"
    , "  processCtx     = **SOME PROC CONTEXT**"
    , "  logFunc        = **SOME LOG FUNCTION**"
    , "  httpManager    = **SOME HTTP MANAGER**"
    , "  ipfsPath       = " <> show ipfsPath
    , "  ipfsURL        = " <> show ipfsURL
    , "  ipfsTimeout    = " <> show ipfsTimeout
    , "  host           = " <> show host
    , "  pgConnectInfo  = " <> show pgConnectInfo
    , "  dbPool         = " <> show dbPool
    , "  herokuID       = " <> show herokuID
    , "  herokuPassword = " <> show herokuPassword
    , "  awsAccessKey   = " <> show awsAccessKey
    , "  awsSecretKey   = HIDDEN"
    , "  awsZoneID      = " <> show awsZoneID
    , "  awsDomainName  = " <> show awsDomainName
    , "}"
    ]

instance HasProcessContext Config where
  processContextL = field @"processCtx"

instance HasLogFunc Config where
  logFuncL = field @"logFunc"

instance Has HTTP.Manager Config where
  hasLens = field @"httpManager"

instance Has IPFS.BinPath Config where
  hasLens = field @"ipfsPath"

instance Has IPFS.URL Config where
  hasLens = field @"ipfsURL"

instance Has IPFS.Timeout Config where
  hasLens = field @"ipfsTimeout"

instance Has PGConnectInfo Config where
  hasLens = field @"pgConnectInfo"

instance Has DB.Pool Config where
  hasLens = field @"dbPool"

instance Has Heroku.ID Config where
  hasLens = field @"herokuID"

instance Has Heroku.Password Config where
  hasLens = field @"herokuPassword"

instance Has AWS.AccessKey Config where
  hasLens = field @"awsAccessKey"

instance Has AWS.SecretKey Config where
  hasLens = field @"awsSecretKey"

instance Has AWS.ZoneID Config where
  hasLens = field @"awsZoneID"

instance Has AWS.DomainName Config where
  hasLens = field @"awsDomainName"

instance Has Host Config where
  hasLens = field @"host"
