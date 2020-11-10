module Fission.Web.User.Link
  ( API
  , socket
  ) where

import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi

import qualified Network.WebSockets                    as WS

import           Servant
import           Servant.API.WebSocket

import           Fission.Prelude

import           Fission.User.DID.Types

type API
  = "link"
  :> Capture "did" DID
  :> WebSocket

socket ::
  ( MonadLogger m
  , MonadIO     m
  -- , MonadLinkRelay m
  )
  => ServerT API m
socket did conn = do
  writeSide <- getRoomFor did -- FIXME line below
  -- writeSide <- ensureM $ getRoomFor did

  liftIO $ WS.withPingThread conn 30 (pure ()) do
    readSide <- Unagi.dupChan writeSide
    concurrently_ (outbound readSide) (inbound writeSide)

  where
    outbound readSide =
      forever do
        msg <- Unagi.readChan readSide
        WS.sendDataMessage conn msg

    inbound writeSide =
      forever do
        msg <- WS.receiveDataMessage conn
        Unagi.writeChan writeSide msg

gtRoomFor :: Monad m => DID -> m (Either (NotFound RelayChan) RelayChan)
getRoomFor did = undefined -- FIXME

newtype RelayChan = RelayChan { getRelayChan :: Unagi.InChan WS.DataMessage }

-- FIXME move to top level & include in Fission.Config.Types
newtype LinkRelay = LinkRelay
  { getRelays :: HashMap DID (Unagi.InChan WS.DataMessage) }
