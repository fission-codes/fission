module Fission.IPFS.DNSLink.Class (MonadDNSLink (..)) where

import           Network.IPFS.CID.Types
import           Servant

import           Fission.AWS.Route53.Class
import           Fission.URL.Types as URL
import           Fission.Prelude

class MonadRoute53 m => MonadDNSLink m where
  set :: Maybe URL.Subdomain -> CID -> m (Either ServerError URL.DomainName)
