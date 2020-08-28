-- | Servant client for retrieving peer data
module Fission.Web.Client.Peers
  ( API
  , getPeers
  ) where

import qualified Network.IPFS.Types         as IPFS

import           Servant.API
import           Servant.Client

import           Fission.Prelude

import           Fission.Web.Client
import qualified Fission.Web.IPFS.Peer      as Peer

import qualified Fission.CLI.Display.Cursor as Cursor
import qualified Fission.CLI.Display.Wait   as CLI.Wait

-- | API path to the peers endpoints
type API = "ipfs" :> "peers" :> Peer.API

-- | Retrieves the Fission peer list from the server
getPeers ::
  ( MonadIO        m
  , MonadWebClient m
  , MonadCleanup   m
  , m `Raises` ClientError
  )
  => m (NonEmpty IPFS.Peer)
getPeers =
  Cursor.withHidden $ CLI.Wait.waitFor "Retrieving Fission Peer List..." do
    ensureM . sendRequest . client $ Proxy @API
