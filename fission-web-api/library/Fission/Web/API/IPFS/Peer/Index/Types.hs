module Fission.Web.API.IPFS.Peer.Index.Types (Index) where

import qualified Network.IPFS.Peer.Types as IPFS

import           Fission.Web.API.Prelude

type Index
  =  Summary "Peer index"
  :> Description "List of recommended IPFS peers"
  --
  :> Get '[JSON, PlainText, OctetStream] (NonEmpty IPFS.Peer)
