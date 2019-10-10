module Fission.Web.DNS.Client
  ( update
  ) where

import RIO

import Servant
import Servant.Client

import           Fission.IPFS.CID.Types
import qualified Fission.Web.Routes as Routes
import qualified Fission.AWS.Types as AWS

update :: BasicAuthData -> CID -> ClientM AWS.DomainName
update = client (Proxy :: Proxy Routes.DNSRoute)