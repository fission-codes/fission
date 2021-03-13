module Fission.Web.Server.Handler.IPFS.Peer (get) where

import           Servant

import qualified Fission.Web.API.IPFS.Peer.Types as API.IPFS
import           Fission.Web.Server.IPFS.Linked

-- | Get a list of valid IPFS addresses that a user could use to join our network
get :: MonadLinkedIPFS m => ServerT API.IPFS.Peer m
get = getLinkedPeers
