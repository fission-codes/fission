module Fission.IPFS.DNSLink.Class (MonadDNSLink (..)) where

import           Network.IPFS.CID.Types
import           Servant

import           Fission.AWS.Route53.Class
import           Fission.AWS.Types as AWS
import           Fission.Prelude

class MonadRoute53 m => MonadDNSLink m where
  set :: Maybe AWS.Subdomain -> CID -> m (Either ServerError AWS.DomainName)
