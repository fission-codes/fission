module Fission.Web.Server.IPFS.DNSLink
 ( toDNSLink
 , module Fission.Web.Server.IPFS.DNSLink.Class
 ) where

import           Fission.Prelude

import           Fission.URL                           as URL

import           Fission.Web.Server.IPFS.DNSLink.Class

toDNSLink :: URL -> URL
toDNSLink url = URL.prefix' (URL.Subdomain "_dnslink") url
