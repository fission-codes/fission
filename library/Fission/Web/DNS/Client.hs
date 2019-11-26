module Fission.Web.DNS.Client (update) where

import Servant
import Servant.Client

import           Fission.Prelude
import qualified Fission.AWS.DomainName.Types as AWS
import           Network.IPFS.CID.Types
import qualified Fission.Web.Routes           as Routes

update :: BasicAuthData -> CID -> ClientM AWS.DomainName
update = client <| Proxy @Routes.DNSRoute
