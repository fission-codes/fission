-- | Servant client for retrieving peer data
module Fission.Web.Client.Peers (getPeers) where

import Fission.Prelude

import Servant
import Servant.Client

import qualified Fission.Web.IPFS.Peer as Peer

import           Fission.Web.Client

import qualified Network.IPFS.Types as IPFS

import qualified Fission.CLI.Display.Cursor as Cursor
import qualified Fission.CLI.Display.Wait as CLI.Wait

-- | API path to the peers endpoints
type API = "ipfs" :> "peers" :> Peer.API

-- | Retrieves the Fission peer list from the server
getPeers ::
  ( MonadWebClient m
  , MonadUnliftIO  m
  )
  => m (Either ClientError (NonEmpty IPFS.Peer))
getPeers = 
  Cursor.withHidden . CLI.Wait.waitFor "Retrieving Fission Peer List..."
    <| run get

-- | Retrieve a list of peers from the fission api
get :: ClientM (NonEmpty IPFS.Peer)
get = client (Proxy :: Proxy API)
