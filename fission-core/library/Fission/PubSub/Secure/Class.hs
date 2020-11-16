-- |

module Fission.PubSub.Secure.Class where

import           Data.Kind

import           Fission.Prelude

import qualified Fission.Key.GenData.Family as Key

import           Fission.PubSub.Class

class MonadPubSub m => MonadPubSubSecure m cipher where
  type SecurePayload m cipher expected :: Type

  genSessionKey :: Key.GenData cipher -> m cipher

  toSecurePayload   :: ToJSON   msg => cipher -> msg -> m (SecurePayload m cipher msg)
  fromSecurePayload :: FromJSON msg => cipher -> SecurePayload m cipher msg -> m (Either String msg) -- FIXME
