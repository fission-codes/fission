{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Web.Client.Internal.Orphanage.WebSocket () where

import           RIO
import qualified RIO.ByteString.Lazy        as Lazy
import qualified RIO.Partial                as Partial
import qualified RIO.Text                   as Text

import qualified Data.Binary.Builder        as Builder

import           Network.WebSockets.Client
import           Servant.API.WebSocket
import           Servant.Client.Core
import           Wuss

import           Fission.Web.API.Host.Types

instance (RunClient m, MonadIO m) => HasClient m WebSocket where
  type Client m WebSocket = Host -> Port -> ClientApp () -> m ()

  hoistClientMonad _pxyM _pxyWS nt client' =
    \host port handler -> nt (client' host port handler)

  clientWithRoute _pxyM _pxyWS Request {..} host (Port port) handler =
    liftIO $ runSecureClient (getRawHost host) (Partial.toEnum port) path handler
    where
      path = Text.unpack . decodeUtf8Lenient . Lazy.toStrict $ Builder.toLazyByteString requestPath
