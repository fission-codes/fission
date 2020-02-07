-- | Configuration types
module Fission.Config.Types (Config (..)) where

import qualified Network.HTTP.Client as HTTP

import Data.Pool
import Database.Persist.Sql (SqlBackend)

import qualified Network.AWS.Auth   as AWS
import qualified Network.IPFS.Types as IPFS

import           Fission.Prelude
import qualified Fission.Platform.Heroku.ID.Types       as Heroku
import qualified Fission.Platform.Heroku.Password.Types as Heroku

import qualified Fission.AWS.Types as AWS
import qualified Fission.URL.Types as URL
import           Fission.Web.Types

-- | The top level 'Fission' application 'RIO' configuration
data Config = Config
  { processCtx                :: !ProcessContext
  , logFunc                   :: !LogFunc
  , httpManager               :: !HTTP.Manager
  , ipfsPath                  :: !IPFS.BinPath
  , ipfsURL                   :: !IPFS.URL
  , ipfsRemotePeer            :: !IPFS.Peer
  , ipfsTimeout               :: !IPFS.Timeout
  , ipfsGateway               :: !IPFS.Gateway
  , host                      :: !Host
  , dbPool                    :: !(Pool SqlBackend)
  , herokuID                  :: !Heroku.ID
  , herokuPassword            :: !Heroku.Password
  , awsAccessKey              :: !AWS.AccessKey
  , awsSecretKey              :: !AWS.SecretKey
  , awsZoneID                 :: !AWS.ZoneID
  , awsDomainName             :: !URL.DomainName
  , awsRoute53MockEnabled     :: !AWS.Route53MockEnabled
  , awsCertManagerMockEnabled :: !AWS.CertManagerMockEnabled
  } deriving Generic

instance Show Config where
  show Config {..} = intercalate "\n"
    [ "Config {"
    , "  processCtx                = **SOME PROC CONTEXT**"
    , "  logFunc                   = **SOME LOG FUNCTION**"
    , "  httpManager               = **SOME HTTP MANAGER**"
    , "  ipfsPath                  = " <> show ipfsPath
    , "  ipfsURL                   = " <> show ipfsURL
    , "  ipfsRemotePeer            = " <> show ipfsRemotePeer
    , "  ipfsTimeout               = " <> show ipfsTimeout
    , "  ipfsGateway               = " <> show ipfsGateway
    , "  host                      = " <> show host
    , "  dbPool                    = " <> show dbPool
    , "  herokuID                  = " <> show herokuID
    , "  herokuPassword            = " <> show herokuPassword
    , "  awsAccessKey              = " <> show awsAccessKey
    , "  awsSecretKey              = HIDDEN"
    , "  awsZoneID                 = " <> show awsZoneID
    , "  awsDomainName             = " <> show awsDomainName
    , "  awsRoute53MockEnabled     = " <> show awsRoute53MockEnabled
    , "  awsCertManagerMockEnabled = " <> show awsCertManagerMockEnabled
    , "}"
    ]

instance HasProcessContext Config where
  processContextL = lens processCtx \cfg newProcessCtx ->
    cfg { processCtx = newProcessCtx }

instance HasLogFunc Config where
  logFuncL = lens logFunc \cfg newLogFunc' ->
    cfg { logFunc = newLogFunc' }
