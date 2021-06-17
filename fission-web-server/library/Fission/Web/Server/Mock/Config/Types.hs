module Fission.Web.Server.Mock.Config.Types (Config (..)) where

import           Network.AWS.Route53

import           Network.IPFS.Client.Pin                as Network.Pin
import           Network.IPFS.File.Types                as File
import           Network.IPFS.Process.Error             as Process
import qualified Network.IPFS.Types                     as IPFS

import           Database.Esqueleto
import           Network.Wai                            as Wai

import           Servant
import           Servant.Client
import           Servant.Server.Experimental.Auth

import           Fission.Prelude

import           Fission.URL                            as URL
import           Fission.User.DID.Types
import qualified Fission.Web.API.Heroku.Auth.Types      as Heroku

import qualified Fission.Web.Server.AWS.Types           as AWS
import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.IPFS.DNSLink.Class  as DNSLink
import           Fission.Web.Server.Models

data Config = Config
  { setDNSLink      :: URL.DomainName -> Maybe URL.Subdomain -> IPFS.CID -> Either DNSLink.Errors' URL
  , unsetDNSLink    :: URL.DomainName -> Maybe URL.Subdomain -> Either DNSLink.Errors' ()
  , followDNSLink   :: URL -> Path URL -> Either DNSLink.Errors' ()
  , getBaseDomain   :: URL.DomainName
  , updateRoute53   :: RecordType -> URL -> AWS.ZoneID -> NonEmpty Text -> Natural -> Either ServerError ChangeResourceRecordSetsResponse
  , clearRoute53    :: URL -> Either ServerError ChangeResourceRecordSetsResponse
  , getRoute53      :: URL -> AWS.ZoneID -> Either ServerError ResourceRecordSet
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
