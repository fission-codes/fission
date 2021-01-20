module Fission.Web.Server.Handler.Relay (relay) where

import qualified Network.WebSockets                     as WS
import           Servant

import           Fission.Prelude

import           Fission.Web.Server.Relay               as Relay

import qualified Fission.Web.Server.Handler.Relay.Types as API

relay :: (MonadIO m, MonadLogger m, MonadRelayStore m) => ServerT API.RelayWS m
relay did conn = do
  logDebug $ "Connected to user link for " <> textDisplay did
  storeVar <- getStoreVar
  (sentBufferVar, chanIn, chanOut) <- atomically $ Relay.setup did storeVar

  liftIO $ WS.withPingThread conn 30 noop do
    concurrently_
      (inbound  conn chanIn  sentBufferVar)
      (outbound conn chanOut sentBufferVar)
