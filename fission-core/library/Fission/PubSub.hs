module Fission.PubSub
  ( listenJSON
  , listenRaw
  , broadcastJSON
  , broadcastRaw
  , module Fission.PubSub.Class
  , module Fission.PubSub.Topic.Types
  ) where

import qualified Data.Binary.Builder        as Binary
import qualified RIO.ByteString.Lazy        as Lazy

import           Servant.API

import           Fission.Prelude

import           Fission.PubSub.Class
import           Fission.PubSub.Topic.Types

listenJSON ::
  ( MonadPubSub m
  , MonadRaise  m
  , m `Raises` String
  , FromJSON msg
  )
  => Connection m
  -> m msg
listenJSON conn = do
  lbs <- receiveLBS conn
  ensure $ eitherDecode lbs

listenRaw ::
  ( MonadPubSub m
  , MonadRaise  m
  , m `Raises` Text
  , FromHttpApiData msg
  )
  => Connection m
  -> m msg
listenRaw conn = do
  lbs <- receiveLBS conn
  ensure . parseHeader $ Lazy.toStrict lbs

broadcastJSON ::
  ( MonadPubSub m
  , ToJSON msg
  )
  => Connection m
  -> msg
  -> m ()
broadcastJSON conn msg = sendLBS conn $ encode msg

broadcastRaw ::
  ( MonadPubSub m
  , ToHttpApiData msg
  )
  => Connection m
  -> msg
  -> m ()
broadcastRaw conn msg =
  sendLBS conn . Binary.toLazyByteString $ toEncodedUrlPiece msg
