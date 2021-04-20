module Fission.CLI.PubSub
  ( listenJSON
  , listenRaw
  , broadcastJSON
  , broadcastApiData
  , module Fission.CLI.PubSub.Class
  , module Fission.CLI.PubSub.Topic.Types
  ) where

import qualified Data.Binary.Builder            as Binary
import qualified RIO.ByteString.Lazy            as Lazy

import           Servant.API

import           Fission.Prelude

import qualified Fission.JSON                   as JSON
import qualified Fission.Web.Serialization      as Web.API

import           Fission.CLI.PubSub.Class
import           Fission.CLI.PubSub.Topic.Types

listenJSON ::
  ( MonadPubSub m
  , MonadRaise  m
  , m `Raises` JSON.Error
  , FromJSON msg
  )
  => Connection m
  -> m msg
listenJSON conn = do
  lbs <- receiveLBS conn
  ensure . JSON.betterError $ eitherDecode lbs

listenRaw ::
  ( MonadPubSub m
  , MonadRaise  m
  , m `Raises` Web.API.Error
  , FromHttpApiData msg
  )
  => Connection m
  -> m msg
listenRaw conn = do
  lbs <- receiveLBS conn
  ensure . Web.API.parseHeader $ Lazy.toStrict lbs

broadcastJSON :: (MonadPubSub m, ToJSON msg) => Connection m -> msg -> m ()
broadcastJSON conn msg = sendLBS conn $ encode msg

broadcastApiData :: (MonadPubSub m, ToHttpApiData msg) => Connection m -> msg -> m ()
broadcastApiData conn msg =
  sendLBS conn . Binary.toLazyByteString $ toEncodedUrlPiece msg
