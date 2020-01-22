module Fission.Internal.Mock.Config.Types (Config (..)) where

import Fission.Environment ((.!~))

import           Network.AWS
import           Network.AWS.Route53

import           Control.Monad.Writer
import           Data.Generics.Product
import           Database.Esqueleto

import qualified Network.IPFS.Types as IPFS
import           Servant
import           Servant.Auth

import           Fission.Internal.Mock.Effect

import           Fission.Prelude
import           Fission.IPFS.Linked.Class
import           Fission.Web.Auth.Class
import           Fission.Models
import Fission.IPFS.DNSLink.Class

import Fission.AWS.Route53.Class

import Fission.Internal.Fixture.Time as Fixture
import Fission.URL.DomainName.Types
import qualified Fission.Platform.Heroku.Auth.Types as Heroku

import Fission.URL.Types as URL
import Network.IPFS.Types

import Database.Esqueleto


data Config m = Config
  { setDNSLink     :: Maybe URL.Subdomain -> CID -> m (Either ServerError URL.DomainName)
  , updateRoute53  :: RecordType -> URL.DomainName -> Text -> m (Either ServerError ChangeResourceRecordSetsResponse)
  , now            :: m UTCTime
  , getLinkedPeers :: m (NonEmpty IPFS.Peer)
  , anyVerifier    :: forall a . a -> m (BasicAuth a)
  , userVerifier   :: m (BasicAuth (Entity User))
  , herokuVerifier :: m (BasicAuth Heroku.Auth)
  , dbRunner       :: m (forall n a . n a -> m a)
  }
