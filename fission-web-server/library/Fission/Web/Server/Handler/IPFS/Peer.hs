module Fission.Web.Server.Handler.IPFS.Peer (get) where

import           Data.List.NonEmpty              as NonEmpty

import           Network.IPFS
import           Network.IPFS.Peer               as IPFS.Peer
-- import qualified Network.IPFS.Types              as IPFS

import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.IPFS.Peer.Types as API.IPFS

import qualified Fission.Web.Server.Error        as Web.Err
import           Fission.Web.Server.IPFS.Linked

-- | Get a list of valid IPFS addresses that a user could use to join our network
get ::
  ( MonadLinkedIPFS m
  , MonadLocalIPFS  m
  , MonadLogger     m
  , MonadThrow      m
  )
  => ServerT API.IPFS.Peer m
get = do
  remotePeers <- getLinkedPeers
  getExternalAddress >>= \case
    Right []    -> return remotePeers
    Right peers -> return (remotePeers <> NonEmpty.fromList peers)
    Left err    -> Web.Err.throw err
