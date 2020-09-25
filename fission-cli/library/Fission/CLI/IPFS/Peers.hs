module Fission.CLI.IPFS.Peers (getPeers) where

import qualified Network.IPFS.Types         as IPFS
import           Servant.Client

import           Fission.Prelude

import           Fission.Web.Client

import qualified Fission.CLI.Display.Cursor as Cursor
import qualified Fission.CLI.Display.Wait   as CLI.Wait

import qualified Fission.Web.Client.Peers   as Web.Client

-- | Retrieves the Fission peer list from the server
getPeers ::
  ( MonadIO        m
  , MonadWebClient m
  , MonadLogger    m
  , MonadCleanup   m
  , m `Raises` ClientError
  )
  => m (NonEmpty IPFS.Peer)
getPeers = do
  logDebug @Text "Getting peers remotely"
  Cursor.withHidden $ CLI.Wait.waitFor "Retrieving Fission Peer List..." do
    Web.Client.getPeers
