module Fission.Web.DNS
  ( API
  , server
  ) where

import           Network.IPFS.CID.Types
import           Database.Esqueleto
import           Servant

import           Fission.Prelude
import           Fission.Authorization
import           Fission.Models

import           Fission.IPFS.DNSLink.Class as DNSLink
import qualified Fission.URL.Types          as URL
import           Fission.Web.Error          as Web.Err
import           Fission.User.Username.Types

type API
  =  Summary "Set default app's DNSLink"
  :> Description "DEPRECATED ⛔ Set default app's DNSLink to a CID"
  :> Capture "cid" CID
  :> PutAccepted '[PlainText, OctetStream] URL.DomainName

server :: MonadDNSLink m => Authorization -> ServerT API m
server Authorization {about = Entity _ User {userUsername = Username rawUN}} cid =
  Web.Err.ensureM =<< DNSLink.setBase (URL.Subdomain rawUN) cid
