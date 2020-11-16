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

secureBroadcast ::
  ( MonadPubSubSecure m cipher
  , MonadRaise        m
  , m `Raises` String
  , ToJSON msg
  , ToJSON (SecurePayload m cipher msg)
  )
  => SecureConnection m cipher
  -> msg
  -> m ()
secureBroadcast SecureConnection {..} msg =
  broadcast conn =<< toSecurePayload sessionKey msg

secureListen ::
  ( MonadPubSubSecure m cipher
  , MonadRaise        m
  , m `Raises` String
  , FromJSON msg
  , FromJSON (SecurePayload m cipher msg)
  )
  => SecureConnection m cipher
  -> m msg
secureListen SecureConnection {..} = do
  payload <- listen conn
  ensureM $ fromSecurePayload sessionKey payload
