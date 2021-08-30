module Fission.Web.API.IPFS.CID.Types (Routes (..)) where

import           Fission.Web.API.Prelude

import           Fission.Web.API.IPFS.CID.AllForUser.Types

newtype Routes mode = Routes { allForUser :: mode :- AllForUser }
  deriving Generic
