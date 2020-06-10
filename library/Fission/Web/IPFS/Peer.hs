module Fission.Web.IPFS.Peer
  ( API
  , get
  ) where

import           Data.List.NonEmpty as NonEmpty

import           Servant
import           Network.IPFS
import           Network.IPFS.Peer  as IPFS.Peer
import qualified Network.IPFS.Types as IPFS

import           Fission.IPFS.Linked
import           Fission.Prelude
import qualified Fission.Web.Error  as Web.Err

type API
  =  Summary "Peer index"
  :> Description "List of recommended IPFS peers"
  :> Get '[JSON, PlainText, OctetStream] (NonEmpty IPFS.Peer)

-- | Get a list of valid IPFS addresses that a user could use to join our network
get ::
  ( MonadLinkedIPFS m
  , MonadLocalIPFS  m
  , MonadLogger     m
  , MonadThrow      m
  )
  => ServerT API m
get = do
  remotePeers <- getLinkedPeers
  getExternalAddress >>= \case
    Right []    -> return remotePeers
    Right peers -> return (remotePeers <> NonEmpty.fromList peers)
    Left err    -> Web.Err.throw err
