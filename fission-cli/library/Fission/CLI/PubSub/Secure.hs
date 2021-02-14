module Fission.CLI.PubSub.Secure
  ( secureBroadcastJSON
  , secureListenJSON
  , module Fission.CLI.PubSub.Secure.Class
  , module Fission.CLI.PubSub.Secure.Payload
  , module Fission.CLI.PubSub.Secure.Connection
  ) where

import           Fission.Prelude

import qualified Fission.JSON                         as JSON

import           Fission.CLI.PubSub

import           Fission.CLI.PubSub.Secure.Class
import qualified Fission.CLI.PubSub.Secure.Connection as Secure
import           Fission.CLI.PubSub.Secure.Payload    as Secure.Payload

-- Reexports

import           Fission.CLI.PubSub.Secure.Connection
import           Fission.CLI.PubSub.Secure.Payload

secureListenJSON ::
  ( MonadLogger  m
  , MonadPubSub  m
  , MonadSecured m cipher msg
  , MonadRescue  m

  , m `Raises` JSON.Error
  , m `Raises` Secure.Payload.Error

  , Display (SecurePayload cipher msg)
  )
  => Secure.Connection m cipher
  -> m msg
secureListenJSON Secure.Connection {..} =
  reattempt 100 do
    payload <- listenJSON conn
    logDebug $ "Got raw encrypted payload: " <> textDisplay payload
    ensureM $ fromSecurePayload key payload

secureBroadcastJSON ::
  ( MonadPubSub  m
  , MonadSecured m cipher msg
  )
  => Secure.Connection m cipher
  -> msg
  -> m ()
secureBroadcastJSON Secure.Connection {..} msg =
  broadcastJSON conn =<< toSecurePayload key msg
