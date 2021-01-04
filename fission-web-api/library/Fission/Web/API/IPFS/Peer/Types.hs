module Fission.Web.API.IPFS.Peer.Types (Peer) where

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.IPFS.Peer.Index.Types as IPFS.Peer

type Peer = "peers" :> IPFS.Peer.Index
