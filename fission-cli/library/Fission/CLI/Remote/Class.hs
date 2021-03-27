module Fission.CLI.Remote.Class (MonadRemote (..)) where

import           Fission.Prelude

import           Fission.Web.API.Remote

class Monad m => MonadRemote m where
  getRemote :: m Remote
