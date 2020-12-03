-- |

module Fission.PubSub.Secure
  ( secureBroadcast
  , secureListen
  , module Fission.PubSub.Secure.Class
  , module Fission.PubSub.Secure.SecureConnection
  ) where

import           Fission.Prelude

import           Fission.PubSub
import           Fission.PubSub.Secure.Class
import           Fission.PubSub.Secure.SecureConnection

secureListen ::
  ( MonadPubSubSecure m cipher
  , MonadLogger       m
  , MonadRescue       m
  , m `Raises` String
  , FromJSON msg
  , FromJSON (SecurePayload m cipher msg)
  )
  => SecureConnection m cipher
  -> m msg
secureListen SecureConnection {..} =
  reattempt 100 do
    payload <- listen conn
    logDebug @Text "Got raw encrypted payload"
    ensureM $ fromSecurePayload key payload

secureBroadcast ::
  ( MonadPubSubSecure m cipher
  , ToJSON msg
  , ToJSON (SecurePayload m cipher msg)
  )
  => SecureConnection m cipher
  -> msg
  -> m ()
secureBroadcast SecureConnection {..} msg =
  broadcast conn =<< toSecurePayload key msg
