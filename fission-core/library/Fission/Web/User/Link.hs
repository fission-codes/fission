module Fission.Web.User.Link
  ( API
  , socket
  ) where

import           RIO.Set                     as Set

import qualified Network.WebSockets          as WS

import           Servant
import           Servant.API.WebSocket

import           Fission.Prelude

import           Fission.User.DID.Types
import           Fission.Web.User.Link.Relay as Relay

type API
  = Capture "did" DID
    :> WebSocket

socket ::
  ( MonadIO         m
  , MonadRelayStore m
  )
  => ServerT API m
socket did conn = do
  storeVar <- getStoreVar
  (sentBufferVar, chanIn, chanOut) <- atomicallyM $ setupSTM did storeVar

  liftIO $ WS.withPingThread conn 30 noop do
    concurrently_
      (inbound  conn chanIn  sentBufferVar)
      (outbound conn chanOut sentBufferVar)

setupSTM ::
     DID
  -> TVar Relay.Store
  -> STM (TVar (Set a), ChannelIn, ChannelOut)
setupSTM did storeVar = do
  store            <- readTVar storeVar
  (chanIn, store') <- getOrCreate did store
  chanOut          <- Relay.toReadChan chanIn
  sentBufferVar    <- newTVar Set.empty

  swapTVar storeVar store'
  return (sentBufferVar, chanIn, chanOut)
