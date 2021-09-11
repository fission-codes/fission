module Fission.Web.API.IPFS.Peer.Types (Routes (..)) where

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.IPFS.Peer.Index.Types as IPFS.Peer

newtype Routes mode = Routes { index :: mode :- IPFS.Peer.Index }
  deriving Generic
