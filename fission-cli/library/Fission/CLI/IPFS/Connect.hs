-- | Module for connecting to the Fission IPFS service
module Fission.CLI.IPFS.Connect
  ( swarmConnectWithRetry
  , couldNotSwarmConnect
  ) where

import qualified RIO.NonEmpty             as NonEmpty

import qualified System.Console.ANSI      as ANSI

import           Network.IPFS
import qualified Network.IPFS.Peer        as IPFS.Peer
import qualified Network.IPFS.Types       as IPFS
import           Servant.Client

import           Fission.Prelude

import           Fission.IPFS.Error.Types as IPFS
import qualified Fission.Internal.UTF8    as UTF8
import           Fission.Web.Client

import           Fission.CLI.Display.Text
import           Fission.CLI.IPFS.Peers   as Peers

-- | Connect to the Fission IPFS network with a set amount of retries
swarmConnectWithRetry ::
  ( MonadIO             m
  , MonadBaseControl IO m
  , MonadLogger         m
  , MonadLocalIPFS      m
  , MonadWebClient      m

  , MonadCleanup m
  , m `Raises` IPFS.UnableToConnect
  , m `Raises` ClientError
  , Contains (Errors m) (Errors m)
  )
  => NonEmpty IPFS.Peer
  -> Natural
  -> m ()
swarmConnectWithRetry peers retries = do
  logDebug @Text "🌌🔌 Connecting peers"
  connectTo peers `attemptM` \case
    Right val ->
      return val

    Left err ->
      case retries of
        0 ->
          raise err

        _ -> do
          retryPeers <- Peers.getPeers
          UTF8.putText "🛰 🔁 Unable to connect to the Fission IPFS peer, trying again...\n"
          swarmConnectWithRetry retryPeers (retries - 1)

connectTo :: forall m .
  ( MonadIO             m
  , MonadBaseControl IO m
  , MonadLogger         m
  , MonadLocalIPFS      m
  , MonadRescue         m
  , m `Raises` IPFS.UnableToConnect
  )
  => NonEmpty IPFS.Peer
  -> m ()
connectTo peers = do
  results <- liftBaseWith \runInBase ->
    forConcurrently peers \peer ->
      runInBase $ tryConnect peer

  filterM findFails (NonEmpty.toList results) >>= \case
    [] -> return ()
    _  -> raise IPFS.UnableToConnect

  where
    findFails :: StM m () -> m Bool
    findFails stm =
      attemptM (restoreM stm) \case
        Left  _  -> return True
        Right () -> return False

    tryConnect :: IPFS.Peer -> m ()
    tryConnect peer@(IPFS.Peer peerTxt) =
      IPFS.Peer.connect peer >>= \case
        Left err -> do
          logWarn $ "🛰️ 🛑 Unable to connect to: " <> textDisplay err
          raise IPFS.UnableToConnect

        Right () -> do
          logInfo $ "🛰️ 🎉 Connected to " <> peerTxt
          return ()

-- | Create a could not connect to Fission peer message for the terminal
couldNotSwarmConnect :: (MonadCleanup m, MonadIO m) => m ()
couldNotSwarmConnect = do
  colourized [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red] do
    UTF8.putText "😭 We were unable to connect to the Fission IPFS peer!\n"

  colourized [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Yellow] do
    UTF8.putText "Try checking your connection or logging in again\n"
