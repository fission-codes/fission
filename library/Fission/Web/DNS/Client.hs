module Fission.Web.DNS.Client (update) where

import           Network.IPFS.CID.Types
import           Servant
import           Servant.Client

import           Fission.Prelude
import qualified Fission.URL.DomainName.Types as URL
import qualified Fission.Web.Routes           as Routes

update :: BasicAuthData -> CID -> ClientM URL.DomainName
update = client <| Proxy @Routes.DNSRoute
