-- | Configuration types
module Fission.Config.Types (Config (..)) where

import qualified Network.HTTP.Client as HTTP
import           Network.IPFS.CID.Types

import qualified Network.AWS.Auth   as AWS
import qualified Network.IPFS.Types as IPFS

import           Data.Pool
import           Database.Persist.Sql (SqlBackend)

import           Fission.User.DID.Types

import           Fission.Prelude
import qualified Fission.Platform.Heroku.ID.Types       as Heroku
import qualified Fission.Platform.Heroku.Password.Types as Heroku

import qualified Fission.AWS.Types as AWS
import           Fission.URL.Types as URL
import           Fission.Web.Types

-- | The top level 'Fission' application 'RIO' configuration
data Config = Config
  { processCtx        :: !ProcessContext
  , logFunc           :: !LogFunc
  --
  , httpManager       :: !HTTP.Manager
  , dbPool            :: !(Pool SqlBackend)
  --
  , ipfsPath          :: !IPFS.BinPath
  , ipfsURL           :: !IPFS.URL
  , ipfsRemotePeer    :: !IPFS.Peer
  , ipfsTimeout       :: !IPFS.Timeout
  , ipfsGateway       :: !IPFS.Gateway
  --
  , herokuID          :: !Heroku.ID
  , herokuPassword    :: !Heroku.Password
  --
  , awsAccessKey      :: !AWS.AccessKey
  , awsSecretKey      :: !AWS.SecretKey
  , awsMockRoute53    :: !AWS.MockRoute53
  --
  , baseAppZoneID     :: !AWS.ZoneID
  , appPlaceholder    :: !CID
  , baseAppDomainName :: !URL.DomainName
  --
  , userRootDomain    :: !URL.DomainName
  , userZoneID        :: !AWS.ZoneID
  , defaultDataCID    :: !CID
  --
  , host              :: !Host
  , fissionDID        :: !DID
  , serverZoneID      :: !AWS.ZoneID
  , liveDriveURL      :: !URL
  }

instance Show Config where
  show Config {..} = intercalate "\n"
    [ "Config {"
    , "  processCtx        = **SOME PROC CONTEXT**"
    , "  logFunc           = **SOME LOG FUNCTION**"
    --
    , "  httpManager       = **SOME HTTP MANAGER**"
    , "  dbPool            = " <> show dbPool
    --
    , "  ipfsPath          = " <> show ipfsPath
    , "  ipfsURL           = " <> show ipfsURL
    , "  ipfsRemotePeer    = " <> show ipfsRemotePeer
    , "  ipfsTimeout       = " <> show ipfsTimeout
    , "  ipfsGateway       = " <> show ipfsGateway
    --
    , "  herokuID          = " <> show herokuID
    , "  herokuPassword    = " <> show herokuPassword
    --
    , "  awsAccessKey      = " <> show awsAccessKey
    , "  awsSecretKey      = HIDDEN"
    , "  awsMockRoute53    = " <> show awsMockRoute53
    --
    , "  baseAppZoneID     = " <> show baseAppZoneID
    , "  appPlaceholder    = " <> show appPlaceholder
    , "  baseAppDomainName = " <> show baseAppDomainName
    --
    , "  userRootDomain    = " <> show userRootDomain
    , "  userZoneID        = " <> show userZoneID
    , "  defaultDataCID    = " <> show defaultDataCID
    --
    , "  host              = " <> show host
    , "  fissionDID        = " <> show fissionDID
    , "  serverZoneID      = " <> show serverZoneID
    , "  liveDriveURL      = " <> show liveDriveURL
    , "}"
    ]

instance HasProcessContext Config where
  processContextL = lens processCtx \cfg newProcessCtx ->
    cfg { processCtx = newProcessCtx }

instance HasLogFunc Config where
  logFuncL = lens logFunc \cfg newLogFunc' ->
    cfg { logFunc = newLogFunc' }
