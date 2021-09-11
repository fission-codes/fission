module Fission.Web.Server.Handler.IPFS.Peer (handler) where

import           Servant.Server.Generic

import qualified Fission.Web.API.IPFS.Peer.Types as IPFS.Peer
import           Fission.Web.Server.IPFS.Linked

-- | Get a list of valid IPFS addresses that a user could use to join our network
handler :: MonadLinkedIPFS m => IPFS.Peer.Routes (AsServerT m)
handler = IPFS.Peer.Routes {index = getLinkedPeers}
