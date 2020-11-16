module Fission.Web.User.Link.Relay
  ( getChanFor
  , inbound
  , outbound
  , module Fission.Web.User.Link.Relay.Channel
  , module Fission.Web.User.Link.Relay.Store
  ) where

import qualified RIO.ByteString.Lazy                 as Lazy
import qualified RIO.Set                             as Set

import           Crypto.Hash                         (Digest, SHA1, hash)

import qualified Network.WebSockets                  as WS

import           Fission.Prelude

import           Fission.User.DID.Types

import           Fission.Web.User.Link.Relay.Channel
import           Fission.Web.User.Link.Relay.Store

getChanFor ::
  ( MonadIO         m
  , MonadRelayStore m
  )
  => DID
  -> m ChannelIn
getChanFor did = do
  storeVar <- getStoreVar

  atomicallyM do
    store            <- readTVar storeVar
    (chanIn, store') <- getOrCreate did store
    swapTVar storeVar store'
    return chanIn

inbound ::
     WS.Connection
  -> ChannelIn
  -> TVar (Set (Digest SHA1))
  -> IO ()
inbound conn (ChannelIn chanIn) sentBufferVar =
  forever do
    msg <- WS.receiveDataMessage conn
    atomically do
      modifyTVar' sentBufferVar \msgs -> Set.insert (toFingerprint msg) msgs
      writeTChan chanIn msg

outbound ::
     WS.Connection
  -> ChannelOut
  -> TVar (Set (Digest SHA1))
  -> IO ()
outbound conn (ChannelOut chanOut) sentBufferVar = do
  forever do
    result <- atomically do
      msg        <- readTChan chanOut
      sentBuffer <- readTVar sentBufferVar

      let fingerprint = toFingerprint msg

      if fingerprint `Set.member` sentBuffer
        then do
          modifyTVar' sentBufferVar \msgs -> Set.delete fingerprint msgs
          return Nothing

        else
          return $ Just msg

    -- FIXME siwtch to monadpubsub
    case result of
      Nothing  -> noop
      Just msg -> WS.sendDataMessage conn msg

toFingerprint :: WS.DataMessage -> Digest SHA1
toFingerprint msg =
  hash $ Lazy.toStrict rawMsg
  where
    rawMsg :: Lazy.ByteString
    rawMsg =
      case msg of
        WS.Text   lbs _ -> lbs
        WS.Binary lbs   -> lbs
