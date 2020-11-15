-- |

module Fission.PubSub.Secure
  ( secureBroadcast
  , secureListen
  , module Fission.PubSub.Secure.Class
  ) where

import           Fission.Prelude

import           Fission.PubSub
import           Fission.PubSub.Secure.Class

secureBroadcast ::
  ( MonadPubSubSecure m
  , MonadRaise        m
  , m `Raises` String
  , ToJSON msg
  )
  => msg
  -> m ()
secureBroadcast msg = broadcast $ toPayload msg

secureListen ::
  ( MonadPubSubSecure m
  , MonadRaise        m
  , m `Raises` String
  , FromJSON msg
  )
  => m msg
secureListen = do
  payload <- listen
  ensure $ fromPayload payload


-- toSecure ::
--   ( MonadRandom m
--   , MonadPubSubSecure m
--   , MonadRaise  m
--   , m `Raises` CryptoError
--   , ToJSON msg
--   )
--   => Symmetric.Key AES256
--   -> msg
--   -> m (Payload msg)
-- toSecure aesKey msg = do
--   Symmetric.genIV >>= \case
--     Nothing ->
--       undefined -- FIXME better error
--
--     Just iv -> do
--       secretMessage <- ensure $ Symmetric.encrypt aesKey iv msg
--       return $ Payload {..}
