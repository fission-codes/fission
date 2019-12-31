module Fission.IPFS.DNSLink
 ( module Fission.IPFS.DNSLink.Class
 , setWithSubdomain
 ) where

import           Servant
import           Network.IPFS.CID.Types

import           Fission.Prelude
import qualified Fission.AWS.Types as AWS

import           Fission.IPFS.DNSLink.Class
import qualified Fission.IPFS.DNSLink.Class as DNSLink

setWithSubdomain :: MonadDNSLink m => Text -> CID -> m (Either ServerError AWS.DomainName)
setWithSubdomain subdomain cid = DNSLink.set (Just (AWS.Subdomain subdomain)) cid
