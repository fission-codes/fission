-- | Configuration types
module Fission.Config.Types (Config (..)) where

import qualified Network.HTTP.Client as HTTP

import           Fission.Prelude
import           Fission.Web.Types
import qualified Fission.IPFS.Types            as IPFS
import qualified Fission.Storage.Types         as DB
import qualified Fission.Platform.Heroku.Types as Heroku
import qualified Network.AWS.Auth              as AWS
import qualified Fission.AWS.Types              as AWS

-- | The top level 'Fission' application 'RIO' configuration
data Config dbBackend = Config
  { processCtx     :: !ProcessContext
  , logFunc        :: !LogFunc
  , httpManager    :: !HTTP.Manager
  , ipfsPath       :: !IPFS.BinPath
  , ipfsURL        :: !IPFS.URL
  , ipfsTimeout    :: !IPFS.Timeout
  , ipfsGateway    :: !IPFS.Gateway
  , host           :: !Host
  , dbPool         :: !(DB.Pool dbBackend)
  , herokuID       :: !Heroku.ID
  , herokuPassword :: !Heroku.Password
  , awsAccessKey   :: !AWS.AccessKey
  , awsSecretKey   :: !AWS.SecretKey
  , awsZoneID      :: !AWS.ZoneID
  , awsDomainName  :: !AWS.DomainName
  } deriving Generic

instance Show (Config dbBackend) where
  show Config {..} = intercalate "\n"
    [ "Config {"
    , "  processCtx     = **SOME PROC CONTEXT**"
    , "  logFunc        = **SOME LOG FUNCTION**"
    , "  httpManager    = **SOME HTTP MANAGER**"
    , "  ipfsPath       = " <> show ipfsPath
    , "  ipfsURL        = " <> show ipfsURL
    , "  ipfsTimeout    = " <> show ipfsTimeout
    , "  ipfsGateway    = " <> show ipfsGateway
    , "  host           = " <> show host
    , "  dbPool         = " <> show dbPool
    , "  herokuID       = " <> show herokuID
    , "  herokuPassword = " <> show herokuPassword
    , "  awsAccessKey   = " <> show awsAccessKey
    , "  awsSecretKey   = HIDDEN"
    , "  awsZoneID      = " <> show awsZoneID
    , "  awsDomainName  = " <> show awsDomainName
    , "}"
    ]

instance HasProcessContext (Config db) where
  processContextL = lens processCtx \cfg newProcessCtx ->
    cfg { processCtx = newProcessCtx }

instance HasLogFunc (Config db) where
  logFuncL = lens logFunc \cfg newLogFunc' ->
    cfg { logFunc = newLogFunc' }

instance Has HTTP.Manager (Config db) where
  hasLens = lens httpManager \cfg newHttpManager ->
    cfg { httpManager = newHttpManager }

instance Has IPFS.BinPath (Config db) where
  hasLens = lens ipfsPath \cfg newIPFSPath ->
    cfg { ipfsPath = newIPFSPath }

instance Has IPFS.URL (Config db) where
  hasLens = lens ipfsURL \cfg newIPFSURL ->
    cfg { ipfsURL = newIPFSURL }

instance Has IPFS.Timeout (Config db) where
  hasLens = lens ipfsTimeout \cfg newIPFSTimeout ->
    cfg { ipfsTimeout = newIPFSTimeout }

instance Has IPFS.Gateway (Config db) where
  hasLens = lens ipfsGateway \cfg newIPFSGateway ->
    cfg { ipfsGateway = newIPFSGateway }

instance Has (DB.Pool db) (Config db) where
  hasLens = lens dbPool \cfg newDBPool ->
    cfg { dbPool = newDBPool }

instance Has Heroku.ID (Config db) where
  hasLens = lens herokuID \cfg newHerokuID ->
    cfg { herokuID = newHerokuID }

instance Has Heroku.Password (Config db) where
  hasLens = lens herokuPassword \cfg newHerokuPassword ->
    cfg { herokuPassword = newHerokuPassword }

instance Has AWS.AccessKey (Config db) where
  hasLens = lens awsAccessKey \cfg newAWSAccessKey ->
    cfg { awsAccessKey = newAWSAccessKey }

instance Has AWS.SecretKey (Config db) where
  hasLens = lens awsSecretKey \cfg newAWSSecretKey ->
    cfg { awsSecretKey = newAWSSecretKey }

instance Has AWS.ZoneID (Config db) where
  hasLens = lens awsZoneID \cfg newAWSZoneID ->
    cfg { awsZoneID = newAWSZoneID }

instance Has AWS.DomainName (Config db) where
  hasLens = lens awsDomainName \cfg newAWSDomainName ->
    cfg { awsDomainName = newAWSDomainName }

instance Has Host (Config db) where
  hasLens = lens host \cfg newHost ->
    cfg { host = newHost }
