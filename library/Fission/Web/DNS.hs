module Fission.Web.DNS
  ( API
  , server
  ) where

import           Network.IPFS.CID.Types
import           Database.Esqueleto
import           Servant

import           Fission.Prelude
import           Fission.Models

import qualified Fission.AWS.Types          as AWS
import           Fission.IPFS.DNSLink.Class as DNSLink
import           Fission.Web.Error          as Web.Err

type API = Capture "cid" CID
        :> PutAccepted '[PlainText, OctetStream] AWS.DomainName

server :: MonadDNSLink m => Entity User -> ServerT API m
server (Entity _id User { userUsername }) cid =
  cid
    |> DNSLink.set (Just (AWS.Subdomain userUsername))
    |> Web.Err.ensureM
