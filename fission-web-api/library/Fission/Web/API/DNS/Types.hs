module Fission.Web.API.DNS.Types (DNS) where

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.DNS.Set.Types as DNS

-- | Top-level DNS web API
type DNS = "dns" :> DNS.Set
