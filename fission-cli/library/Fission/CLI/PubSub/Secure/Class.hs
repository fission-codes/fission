module Fission.CLI.PubSub.Secure.Class (MonadPubSubSecure (..)) where

import qualified Fission.Key.GenData.Family as Key

import           Fission.CLI.PubSub.Class

class MonadPubSub m => MonadPubSubSecure m cipher where
  genSessionKey :: Key.GenData cipher -> m cipher
