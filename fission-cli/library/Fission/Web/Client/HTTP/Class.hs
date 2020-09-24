module Fission.Web.Client.HTTP.Class (MonadManagedHTTP (..)) where

import qualified Network.HTTP.Client as HTTP

import           Fission.Prelude

class Monad m => MonadManagedHTTP m where
  getHTTPManager :: m HTTP.Manager
