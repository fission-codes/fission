module Fission.PubSub.Secure.SecureConnection
  ( secureConnection
  , module Fission.PubSub.Secure.SecureConnection.Types
  ) where

import qualified Fission.Key.GenData.Family                   as Key

import           Fission.PubSub.Class
import           Fission.PubSub.Secure.Class

import           Fission.PubSub.Secure.SecureConnection.Types

-- | Add a secure channel over the existing connection
secureConnection ::
  MonadPubSubSecure m cipher
  => Connection m
  -> Key.GenData cipher
  -> (SecureConnection m cipher -> m a)
  -> m a
secureConnection conn genData withSecureConn = do
  key <- genSessionKey genData
  withSecureConn SecureConnection {..}
