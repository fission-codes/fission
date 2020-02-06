module Fission.Web.DNS
  ( API
  , server
  ) where

import           Servant

import           Fission.IPFS.DNSLink.Class as DNSLink

import           Fission.Web.Auth.Types     as Auth
import qualified Fission.Web.DNS.Create as DNS.Create
import qualified Fission.Web.DNS.Update as DNS.Update

type API = Auth.ExistingUser :> UnauthedAPI
type UnauthedAPI = DNS.Create.API
              :<|> DNS.Update.API

server :: MonadDNSLink m => ServerT API m
server usr = DNS.Create.server usr
        :<|> DNS.Update.server usr
