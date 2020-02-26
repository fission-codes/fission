module Fission.IPFS.DNSLink
 ( module Fission.IPFS.DNSLink.Class
 , setWithSubdomain
 ) where

import           Servant
import           Network.IPFS.CID.Types

import           Fission.Prelude
import qualified Fission.URL.Types as URL

import           Fission.IPFS.DNSLink.Class
import qualified Fission.IPFS.DNSLink.Class as DNSLink

-- FIXME looks exactly the same as what's in the class
setWithSubdomain :: MonadDNSLink m => Text -> CID -> m (Either ServerError URL.DomainName)
setWithSubdomain subdomain cid = DNSLink.set (Just (URL.Subdomain subdomain)) cid
