-- | Module for connecting to the Fission IPFS service
module Fission.CLI.IPFS.Connect
  ( swarmConnectWithRetry
  , couldNotSwarmConnect
  ) where

import           Fission.Prelude

import qualified System.Console.ANSI as ANSI
import           Data.List.NonEmpty as NonEmpty hiding ((<|))

import           Fission.Web.Client
import           Fission.Web.Client.Peers as Peers

import qualified Fission.Internal.UTF8 as UTF8

import           Fission.CLI.IPFS.Error.Types

import           Network.IPFS
import qualified Network.IPFS.Peer  as IPFS.Peer
import qualified Network.IPFS.Types as IPFS


-- | Connect to the Fission IPFS network with a set amount of retries
swarmConnectWithRetry ::
  ( MonadUnliftIO  m
  , MonadLogger    m
  , MonadWebClient m
  , MonadLocalIPFS m
  )
  => IPFS.Peer
  -> Int
  -> m (Either SomeException ())
swarmConnectWithRetry _peer (-1) = return <| Left <| toException UnableToConnect
swarmConnectWithRetry peer tries = IPFS.Peer.connect peer >>= \case
  Right _ ->
    return <| Right ()

  Left _err ->
    Peers.getPeers >>= \case
      Left _ ->
        return <| Left <| toException UnableToConnect

      Right peers -> do
        UTF8.putText "🛰 Unable to connect to the Fission IPFS peer, trying again...\n"
        let peer' = head peers
        swarmConnectWithRetry peer' (tries - 1)

-- | Create a could not connect to Fission peer message for the terminal
couldNotSwarmConnect :: MonadIO m => m ()
couldNotSwarmConnect = do
  liftIO <| ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
  UTF8.putText "😭 We were unable to connect to the Fission IPFS peer!\n"

  liftIO <| ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
  UTF8.putText "Try checking your connection or logging in again\n"

  liftIO <| ANSI.setSGR [ANSI.Reset]
