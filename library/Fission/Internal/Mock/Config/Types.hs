module Fission.Internal.Mock.Config.Types (Config (..)) where

import           Network.AWS.Route53

import qualified Network.IPFS.Types         as IPFS
import           Network.IPFS.Process.Error as Process

import Network.IPFS.File.Types as File

import Network.IPFS.Client.Pin as Network.Pin

import           Database.Esqueleto
import           Servant
import           Servant.Client

import           Fission.Models
import           Fission.Prelude
import qualified Fission.Platform.Heroku.Auth.Types as Heroku
import           Fission.URL.Types as URL

data Config = Config
  { setDNSLink       :: Maybe URL.Subdomain -> IPFS.CID -> (Either ServerError URL.DomainName)
  , updateRoute53    :: RecordType -> URL.DomainName -> Text -> (Either ServerError ChangeResourceRecordSetsResponse)
  , createHostedZone :: URL.DomainName -> (Either ServerError CreateHostedZoneResponse)
  , now              :: UTCTime
  , linkedPeers      :: NonEmpty IPFS.Peer
  , userVerifier     :: BasicAuthCheck (Entity User)
  , herokuVerifier   :: BasicAuthCheck Heroku.Auth
  , forceAuthed      :: Bool
  , localIPFSCall    :: Either Process.Error Process.RawMessage
  , remoteIPFSAdd    :: Either ClientError IPFS.CID
  , remoteIPFSCat    :: Either ClientError File.Serialized
  , remoteIPFSPin    :: Either ClientError Network.Pin.Response
  , remoteIPFSUnpin  :: Either ClientError Network.Pin.Response
  }
