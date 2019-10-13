module Fission.Web.DNS.Client (update) where

import RIO

import Servant
import Servant.Client

import qualified Fission.AWS.Types      as AWS
import           Fission.IPFS.CID.Types
import           Fission.Web.Routes

update :: BasicAuthData -> CID -> ClientM AWS.DomainName
update = client (Proxy :: Proxy DNSRoute)
