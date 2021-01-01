-- |

module Fission.PubSub.Secure
  ( secureBroadcastJSON
  , secureListenJSON
  , module Fission.PubSub.Secure.Class
  , module Fission.PubSub.Secure.SecureConnection
  ) where

import           Fission.Prelude

import           Fission.PubSub
import           Fission.PubSub.Secure.Class
import           Fission.PubSub.Secure.SecureConnection

secureListenJSON ::
  ( MonadPubSubSecure m cipher
  , MonadLogger       m
  , MonadRescue       m
  , m `Raises` String
  , FromJSON msg
  , FromJSON (SecurePayload m cipher msg)
  , Display  (SecurePayload m cipher msg)
  )
  => SecureConnection m cipher
  -> m msg
secureListenJSON SecureConnection {..} =
  reattempt 100 do -- FIXME reply with something if fails
    payload <- listenJSON conn
    logDebug $ "Got raw encrypted payload: " <> textDisplay payload
    ensureM $ fromSecurePayload key payload

secureBroadcastJSON ::
  ( MonadPubSubSecure m cipher
  , Show msg
  , ToJSON msg
  , ToJSON (SecurePayload m cipher msg)
  )
  => SecureConnection m cipher
  -> msg
  -> m ()
secureBroadcastJSON SecureConnection {..} msg =
  broadcastJSON conn =<< toSecurePayload key msg
