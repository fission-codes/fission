module Fission.Web.API.IPFS.CID.Types (CID) where

import           Fission.Web.API.Prelude

import           Fission.Web.API.IPFS.CID.AllForUser.Types

-- | Entry to CID routes
type CID = "cids" :> AllForUser
