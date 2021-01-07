module Fission.CLI.PubSub.Secure.Connection
  ( secure
  , module Fission.CLI.PubSub.Secure.Connection.Types
  ) where

import           Data.Typeable                              (typeOf)
import qualified RIO.Text                                   as Text

import           Fission.Prelude

import qualified Fission.Key.GenData.Family                 as Key

import qualified Fission.CLI.PubSub.Class                   as Insecure
import           Fission.CLI.PubSub.Secure.Class
import qualified Fission.CLI.PubSub.Secure.Connection.Types as Secure

-- Reexports

import           Fission.CLI.PubSub.Secure.Connection.Types

-- | Add a secure channel over the existing connection
secure ::
  ( MonadLogger       m
  , MonadPubSubSecure m cipher
  , Typeable            cipher
  )
  => Insecure.Connection m
  -> Key.GenData cipher
  -> (Secure.Connection m cipher -> m a)
  -> m a
secure conn genData withSecureConn = do
  key <- genSessionKey genData
  logDebug $ "Opening secure channel over " <> Text.pack (show $ typeOf key)
  withSecureConn Secure.Connection {..}
