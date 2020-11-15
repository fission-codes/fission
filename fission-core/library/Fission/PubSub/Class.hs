module Fission.PubSub.Class
  ( MonadPubSub (..)
  , module Fission.PubSub.Topic.Types
  ) where

import           Data.Kind

import           Fission.Prelude

import           Fission.PubSub.Topic.Types

class Monad m => MonadPubSub m where
  type Connection m :: Type

  connect :: Topic -> (Connection m -> m a) -> m a
  getConnection :: m (Connection m)

  send    :: ByteString -> m ()
  receive :: m ByteString

instance MonadPubSub m => MonadPubSub (ReaderT cfg m) where
  type Connection (ReaderT cfg m) = Connection m

  connect topic withConn =
    ReaderT \cfg ->
      connect topic \conn ->
        runReaderT (withConn conn) cfg

  send msg = lift $ send msg
  receive  = lift receive
