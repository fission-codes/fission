module Fission.Internal.Mock.Config.Types (Config (..)) where

import           Network.AWS.Route53

import qualified Network.IPFS.Types         as IPFS
import           Network.IPFS.Process.Error as Process
import           Network.IPFS.File.Types    as File
import           Network.IPFS.Client.Pin    as Network.Pin

import           Database.Esqueleto
import           Network.Wai as Wai

import           Servant
import           Servant.Client
import           Servant.Server.Experimental.Auth

import           Fission.Models
import           Fission.Prelude
import qualified Fission.Platform.Heroku.Auth.Types as Heroku
import           Fission.URL.Types as URL
import           Fission.PublicKey.Types

data Config = Config
  { setDNSLink      :: URL.DomainName -> Maybe URL.Subdomain -> IPFS.CID -> (Either ServerError URL.DomainName)
  , getBaseDomain   :: URL.DomainName
  , updateRoute53   :: RecordType -> URL.DomainName -> Text -> (Either ServerError ChangeResourceRecordSetsResponse)
  , now             :: UTCTime
  , linkedPeers     :: NonEmpty IPFS.Peer
  , userVerifier    :: AuthHandler Wai.Request (Entity User)
  , pkVerifier      :: AuthHandler Wai.Request PublicKey
  , herokuVerifier  :: BasicAuthCheck Heroku.Auth
  , forceAuthed     :: Bool
  , localIPFSCall   :: Either Process.Error Process.RawMessage
  , remoteIPFSAdd   :: Either ClientError IPFS.CID
  , remoteIPFSCat   :: Either ClientError File.Serialized
  , remoteIPFSPin   :: Either ClientError Network.Pin.Response
  , remoteIPFSUnpin :: Either ClientError Network.Pin.Response
  }
