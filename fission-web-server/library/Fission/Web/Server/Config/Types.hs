-- | Configuration types
module Fission.Web.Server.Config.Types (Config (..)) where

import qualified Network.HTTP.Client                       as HTTP
import           Network.IPFS.CID.Types

import qualified Network.AWS.Auth                          as AWS
import qualified Network.IPFS.Types                        as IPFS

import           Data.Pool
import           Database.Persist.Sql                      (SqlBackend)

import           Fission.Prelude

import           Fission.URL.Types                         as URL
import           Fission.User.DID.Types

import           Fission.Web.Server.Host.Types

import qualified Fission.Web.Server.AWS.Types              as AWS

import qualified Fission.Web.Server.Heroku.ID.Types        as Heroku
import qualified Fission.Web.Server.Heroku.Password.Types  as Heroku

import qualified Fission.Web.Server.Email.SendInBlue.Types as SIB
import qualified Fission.Web.Server.Relay.Store.Types      as Relay

-- | The top level 'Fission' application 'RIO' configuration
data Config = Config
  { processCtx        :: ProcessContext
  , logFunc           :: LogFunc
  --
  , httpManager       :: HTTP.Manager
  , tlsManager        :: HTTP.Manager
  , ipfsHttpManager   :: HTTP.Manager
  , dbPool            :: Pool SqlBackend
  --
  , ipfsPath          :: IPFS.BinPath
  , ipfsURLs          :: NonEmpty IPFS.URL
  , ipfsRemotePeers   :: NonEmpty IPFS.Peer
  , ipfsTimeout       :: IPFS.Timeout
  --
  , herokuID          :: Heroku.ID
  , herokuPassword    :: Heroku.Password
  --
  , awsAccessKey      :: AWS.AccessKey
  , awsSecretKey      :: AWS.SecretKey
  , awsMockRoute53    :: AWS.MockRoute53
  --
  , baseAppDomain     :: URL.DomainName
  , baseAppZoneID     :: AWS.ZoneID
  , appPlaceholder    :: CID
  --
  , userRootDomain    :: URL.DomainName
  , userZoneID        :: AWS.ZoneID
  , defaultDataCID    :: CID
  --
  , sibApiKey         :: SIB.ApiKey
  , sibUrl            :: Host
  , sibTemplateId     :: SIB.TemplateId
  --
  , host              :: Host
  , fissionDID        :: DID
  , serverZoneID      :: AWS.ZoneID
  , liveDriveURL      :: URL
  --
  , linkRelayStoreVar :: TVar Relay.Store
  }

instance Show Config where
  show Config {..} = intercalate "\n"
    [ "Config {"
    , "  processCtx        = **SOME PROC CONTEXT**"
    , "  logFunc           = **SOME LOG FUNCTION**"
    --
    , "  httpManager       = **SOME HTTP MANAGER**"
    , "  tlsManager        = **SOME HTTP/TLS MANAGER**"
    , "  ipfsHttpManager   = **SOME HTTP/TLS MANAGER**"
    , "  dbPool            = " <> show dbPool
    --
    , "  ipfsPath          = " <> show ipfsPath
    , "  ipfsURLs          = " <> show ipfsURLs
    , "  ipfsRemotePeers   = " <> show ipfsRemotePeers
    , "  ipfsTimeout       = " <> show ipfsTimeout
    --
    , "  herokuID          = " <> show herokuID
    , "  herokuPassword    = " <> show herokuPassword
    --
    , "  awsAccessKey      = " <> show awsAccessKey
    , "  awsSecretKey      = HIDDEN"
    , "  awsMockRoute53    = " <> show awsMockRoute53
    --
    , "  baseAppZoneID     = " <> show baseAppZoneID
    , "  baseAppDomainName = " <> show baseAppDomain
    , "  appPlaceholder    = " <> show appPlaceholder
    --
    , "  userRootDomain    = " <> show userRootDomain
    , "  userZoneID        = " <> show userZoneID
    , "  defaultDataCID    = " <> show defaultDataCID
    --
    , "  sibApiKey         = " <> show sibApiKey
    , "  sibUrl            = " <> show sibUrl
    , "  sibTemplateId     = " <> show sibTemplateId
    --
    , "  host              = " <> show host
    , "  fissionDID        = " <> show fissionDID
    , "  serverZoneID      = " <> show serverZoneID
    , "  liveDriveURL      = " <> show liveDriveURL
    --
    , "  linkRelayStoreVar = **SOME RELAY STORE TVAR**"
    , "}"
    ]

instance HasProcessContext Config where
  processContextL = lens processCtx \cfg newProcessCtx ->
    cfg { processCtx = newProcessCtx }

instance HasLogFunc Config where
  logFuncL = lens logFunc \cfg newLogFunc' ->
    cfg { logFunc = newLogFunc' }
