module Fission.Web.API.DNS.Set.Types (Set) where

import           Network.IPFS.CID.Types     (CID)

import           Fission.URL                (DomainName)

import           Fission.Web.API.Prelude    hiding (Set)

import qualified Fission.Web.API.Auth.Types as Auth

type Set
  = Auth.HigherOrder
  --
  :> Summary "Set account's DNSLink"
  :> Description "DEPRECATED ⛔ Set account's DNSLink to a CID"
  --
  :> Capture "cid" CID
  :> PutAccepted '[PlainText, OctetStream] DomainName
