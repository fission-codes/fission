module Fission.Web.DNS
  ( API
  , server
  ) where

import           Network.IPFS.CID.Types
import           Database.Esqueleto
import           Servant

import           Fission.Prelude
import           Fission.Models

import           Fission.IPFS.DNSLink.Class as DNSLink
import qualified Fission.URL.Types          as URL
import           Fission.Web.Error          as Web.Err
import           Fission.User.Username.Types

type API
  =  Summary "Set default app's DNSLink"
  :> Description "[DEPRECATED] Set default app's DNSLink to a CID"
  :> Capture "cid" CID
  :> PutAccepted '[PlainText, OctetStream] URL.DomainName

server :: MonadDNSLink m => Entity User -> ServerT API m
server (Entity _id User { userUsername = Username rawUN}) cid =
  cid
    |> DNSLink.setBase (URL.Subdomain rawUN)
    |> bind Web.Err.ensureM
