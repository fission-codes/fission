module Fission.PubSub.DM.Class (MonadPubSubDM (..)) where

import qualified Crypto.PubKey.RSA.Types as RSA

import           Fission.Prelude

import           Fission.PubSub.Class

class MonadPubSub m => MonadPubSubDM m where
  withDMKey :: RSA.PrivateKey -> m a -> m a
  getDMKey  :: m (RSA.PrivateKey)

-- instance MonadPubSubDM m => MonadPubSubDM (ReaderT cfg m) where
  -- withDMKey sk action =
