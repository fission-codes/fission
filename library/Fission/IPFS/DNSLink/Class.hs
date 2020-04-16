module Fission.IPFS.DNSLink.Class (MonadDNSLink (..)) where

import           Network.IPFS.CID.Types
import           Servant

import           Fission.AWS.Route53.Class
import           Fission.URL.Types as URL
import           Fission.Prelude hiding (set)

class MonadRoute53 m => MonadDNSLink m where
  set :: DomainName -> Maybe URL.Subdomain -> CID -> m (Either ServerError URL.DomainName)
  setBase :: URL.Subdomain -> CID -> m (Either ServerError URL.DomainName)

instance MonadDNSLink m => MonadDNSLink (Transaction m) where
  set domainName maySubdomain cid = lift $ set domainName maySubdomain cid
  setBase subdomain cid = lift $ setBase subdomain cid
