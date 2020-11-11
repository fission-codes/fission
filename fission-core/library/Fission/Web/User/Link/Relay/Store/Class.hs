module Fission.Web.User.Link.Relay.Store.Class where

import           Fission.Prelude

import           Fission.Web.User.Link.Relay.Store.Types

class Monad m => MonadRelayStore m where
  getStoreVar :: m (TVar Store)
