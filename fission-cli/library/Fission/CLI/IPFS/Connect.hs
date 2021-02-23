-- | Module for connecting to the Fission IPFS service
module Fission.CLI.IPFS.Connect
  ( swarmConnectWithRetry
  , couldNotSwarmConnect
  ) where

import qualified RIO.NonEmpty                as NonEmpty

import           Control.Parallel.Strategies (parMap, rpar)

import qualified System.Console.ANSI         as ANSI

import           Network.IPFS
import qualified Network.IPFS.Peer           as IPFS.Peer
import qualified Network.IPFS.Types          as IPFS
import           Servant.Client

import           Fission.Prelude

import qualified Fission.Internal.UTF8       as UTF8
import           Fission.IPFS.Error.Types    as IPFS
import           Fission.Web.Client

import           Fission.CLI.Display.Text
import           Fission.CLI.IPFS.Peers      as Peers

-- | Connect to the Fission IPFS network with a set amount of retries
swarmConnectWithRetry ::
  ( MonadIO        m
  , MonadLogger    m
  , MonadLocalIPFS m
  , MonadWebClient m

  , MonadCleanup m
  , m `Raises` IPFS.UnableToConnect
  , m `Raises` ClientError
  , Contains (Errors m) (Errors m)
  )
  => NonEmpty IPFS.Peer
  -> Natural
  -> m ()
swarmConnectWithRetry peers retries = do
  logDebug @Text "Connecting peers"
  connectTo peers `rescue` \err ->
    case retries of
      0 ->
        raise err

      _ -> do
        retryPeers <- Peers.getPeers
        swarmConnectWithRetry retryPeers (retries - 1)

connectTo ::
  ( MonadIO        m
  , MonadLogger    m
  , MonadLocalIPFS m
  , MonadRescue    m
  , m `Raises` IPFS.UnableToConnect
  )
  => NonEmpty IPFS.Peer
  -> m ()
connectTo peers = do
  sequence (logDebug . show <$> NonEmpty.toList peers)
  attempts <- sequence . parMap rpar IPFS.Peer.connect $ NonEmpty.toList peers

  if any isRight attempts
    then do
      logDebug @Text "Successfully connected to a node. Full results:"
      logDebug $ show attempts

    else do
      logDebug @Text "Unable to connect. Full results:"
      logDebug $ show attempts

      UTF8.putText "🛰 Unable to connect to the Fission IPFS peer, trying again...\n"
      raise IPFS.UnableToConnect

-- | Create a could not connect to Fission peer message for the terminal
couldNotSwarmConnect :: (MonadCleanup m, MonadIO m) => m ()
couldNotSwarmConnect = do
  colourized [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red] do
    UTF8.putText "😭 We were unable to connect to the Fission IPFS peer!\n"

  colourized [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Yellow] do
    UTF8.putText "Try checking your connection or logging in again\n"

