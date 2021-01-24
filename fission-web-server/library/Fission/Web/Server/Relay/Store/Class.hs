module Fission.Web.Server.Relay.Store.Class (MonadRelayStore (..)) where

import           Fission.Prelude

import           Fission.Web.Server.Relay.Store.Types

class Monad m => MonadRelayStore m where
  getStoreVar :: m (TVar Store)
