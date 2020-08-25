module Fission.Internal.Mock.Config.Types (Config (..)) where

import           Network.AWS.Route53

import           Network.IPFS.Client.Pin            as Network.Pin
import           Network.IPFS.File.Types            as File
import           Network.IPFS.Process.Error         as Process
import qualified Network.IPFS.Types                 as IPFS

import           Database.Esqueleto
import           Network.Wai                        as Wai

import           Servant
import           Servant.Client
import           Servant.Server.Experimental.Auth

import           Fission.Models
import           Fission.Prelude

import qualified Fission.IPFS.DNSLink.Class         as DNSLink

import           Fission.Authorization.Types
import qualified Fission.AWS.Types                  as AWS
import qualified Fission.Platform.Heroku.Auth.Types as Heroku
import           Fission.URL                        as URL
import           Fission.User.DID.Types

data Config = Config
  { setDNSLink      :: URL.DomainName -> Maybe URL.Subdomain -> IPFS.CID -> Either DNSLink.Errors' URL
  , followDNSLink   :: URL -> Path URL -> Either DNSLink.Errors' ()
  , getBaseDomain   :: URL.DomainName
  , updateRoute53   :: RecordType -> URL -> AWS.ZoneID -> NonEmpty Text -> Natural -> Either ServerError ChangeResourceRecordSetsResponse
  , clearRoute53    :: RecordType -> URL -> Either ServerError ChangeResourceRecordSetsResponse
  , now             :: UTCTime
  , linkedPeers     :: NonEmpty IPFS.Peer
  , userVerifier    :: AuthHandler Wai.Request (Entity User)
  , didVerifier     :: AuthHandler Wai.Request DID
  , authVerifier    :: AuthHandler Wai.Request Authorization
  , herokuVerifier  :: BasicAuthCheck Heroku.Auth
  , forceAuthed     :: Bool
  , localIPFSCall   :: Either Process.Error Process.RawMessage
  , remoteIPFSAdd   :: Either ClientError IPFS.CID
  , remoteIPFSCat   :: Either ClientError File.Serialized
  , remoteIPFSPin   :: Either ClientError Network.Pin.Response
  , remoteIPFSUnpin :: Either ClientError Network.Pin.Response
  }
