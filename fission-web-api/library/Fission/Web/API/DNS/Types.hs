module Fission.Web.API.DNS.Types (Routes (..)) where

import           Network.IPFS.CID.Types     (CID)

import           Fission.URL                (DomainName)

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types as Auth

-- | Top-level DNS web API
data Routes mode = Routes
  { set ::
      mode
      :- Summary "Set account's DNSLink"
      :> Description "DEPRECATED ⛔ Set account's DNSLink to a CID"
      --
      :> Capture "cid" CID
      --
      :> Auth.HigherOrder
      :> PutAccepted '[PlainText, OctetStream] DomainName
  }
  deriving Generic

