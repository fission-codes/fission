-- | Configuration types
module Fission.Config.Types (Config (..)) where

import qualified Network.HTTP.Client as HTTP

import Data.Pool
import Database.Persist.Sql (SqlBackend)

import qualified Network.AWS.Auth   as AWS
import qualified Network.IPFS.Types as IPFS

import           Fission.Prelude
import           Fission.Web.Types
import qualified Fission.Platform.Heroku.ID.Types       as Heroku
import qualified Fission.Platform.Heroku.Password.Types as Heroku
import qualified Fission.AWS.Types                      as AWS


-- | The top level 'Fission' application 'RIO' configuration
data Config = Config
  { processCtx            :: !ProcessContext
  , logFunc               :: !LogFunc
  , httpManager           :: !HTTP.Manager
  , ipfsPath              :: !IPFS.BinPath
  , ipfsURL               :: !IPFS.URL
  , ipfsRemotePeer        :: !IPFS.Peer
  , ipfsTimeout           :: !IPFS.Timeout
  , ipfsGateway           :: !IPFS.Gateway
  , host                  :: !Host
  , dbPool                :: !(Pool SqlBackend)
  , herokuID              :: !Heroku.ID
  , herokuPassword        :: !Heroku.Password
  , awsAccessKey          :: !AWS.AccessKey
  , awsSecretKey          :: !AWS.SecretKey
  , awsZoneID             :: !AWS.ZoneID
  , awsDomainName         :: !AWS.DomainName
  , awsRoute53MockEnabled :: !AWS.Route53MockEnabled
  } deriving Generic

instance Show Config where
  show Config {..} = intercalate "\n"
    [ "Config {"
    , "  processCtx            = **SOME PROC CONTEXT**"
    , "  logFunc               = **SOME LOG FUNCTION**"
    , "  httpManager           = **SOME HTTP MANAGER**"
    , "  ipfsPath              = " <> show ipfsPath
    , "  ipfsURL               = " <> show ipfsURL
    , "  ipfsRemotePeer        = " <> show ipfsRemotePeer
    , "  ipfsTimeout           = " <> show ipfsTimeout
    , "  ipfsGateway           = " <> show ipfsGateway
    , "  host                  = " <> show host
    , "  dbPool                = " <> show dbPool
    , "  herokuID              = " <> show herokuID
    , "  herokuPassword        = " <> show herokuPassword
    , "  awsAccessKey          = " <> show awsAccessKey
    , "  awsSecretKey          = HIDDEN"
    , "  awsZoneID             = " <> show awsZoneID
    , "  awsDomainName         = " <> show awsDomainName
    , "  awsRoute53MockEnabled = " <> show awsRoute53MockEnabled
    , "}"
    ]

instance HasProcessContext Config where
  processContextL = lens processCtx \cfg newProcessCtx ->
    cfg { processCtx = newProcessCtx }

instance HasLogFunc Config where
  logFuncL = lens logFunc \cfg newLogFunc' ->
    cfg { logFunc = newLogFunc' }

instance Has HTTP.Manager Config where
  hasLens = lens httpManager \cfg newHttpManager ->
    cfg { httpManager = newHttpManager }

instance Has IPFS.BinPath Config where
  hasLens = lens ipfsPath \cfg newIPFSPath ->
    cfg { ipfsPath = newIPFSPath }

instance Has IPFS.URL Config where
  hasLens = lens ipfsURL \cfg newIPFSURL ->
    cfg { ipfsURL = newIPFSURL }

instance Has IPFS.Peer Config where
  hasLens = lens ipfsRemotePeer \cfg newIPFSRemotePeer ->
    cfg { ipfsRemotePeer = newIPFSRemotePeer }

instance Has IPFS.Timeout Config where
  hasLens = lens ipfsTimeout \cfg newIPFSTimeout ->
    cfg { ipfsTimeout = newIPFSTimeout }

instance Has IPFS.Gateway Config where
  hasLens = lens ipfsGateway \cfg newIPFSGateway ->
    cfg { ipfsGateway = newIPFSGateway }

instance Has (Pool SqlBackend) Config where
  hasLens = lens dbPool \cfg newDBPool ->
    cfg { dbPool = newDBPool }

instance Has Heroku.ID Config where
  hasLens = lens herokuID \cfg newHerokuID ->
    cfg { herokuID = newHerokuID }

instance Has Heroku.Password Config where
  hasLens = lens herokuPassword \cfg newHerokuPassword ->
    cfg { herokuPassword = newHerokuPassword }

instance Has AWS.AccessKey Config where
  hasLens = lens awsAccessKey \cfg newAWSAccessKey ->
    cfg { awsAccessKey = newAWSAccessKey }

instance Has AWS.SecretKey Config where
  hasLens = lens awsSecretKey \cfg newAWSSecretKey ->
    cfg { awsSecretKey = newAWSSecretKey }

instance Has AWS.ZoneID Config where
  hasLens = lens awsZoneID \cfg newAWSZoneID ->
    cfg { awsZoneID = newAWSZoneID }

instance Has AWS.DomainName Config where
  hasLens = lens awsDomainName \cfg newAWSDomainName ->
    cfg { awsDomainName = newAWSDomainName }

instance Has AWS.Route53MockEnabled Config where
  hasLens = lens awsRoute53MockEnabled \cfg newAWSMockEnabled ->
    cfg { awsRoute53MockEnabled = newAWSMockEnabled }

instance Has Host Config where
  hasLens = lens host \cfg newHost ->
    cfg { host = newHost }
