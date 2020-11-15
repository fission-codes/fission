-- |

module Fission.PubSub.Secure.Class where

import           Data.Kind

import           Fission.Prelude

import           Fission.PubSub.Class

class MonadPubSub m => MonadPubSubSecure m where
  type SessionKey    m :: Type
  type SecurePayload m :: Type -> Type -- Where the first field is the expected return value

  genSessionKey :: m (SessionKey m)

  withSessionKey :: SessionKey m -> (SessionKey m -> m a) -> m a
  getSessionKey  :: m (SessionKey m)


  toSecurePayload   :: msg -> m (SecurePayload m msg)
  fromSecurePayload :: SecurePayload m msg -> m (Either String msg) -- FIXME
