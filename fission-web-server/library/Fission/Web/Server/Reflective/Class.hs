module Fission.Web.Server.Reflective.Class (MonadReflectiveServer (..)) where

import           Fission.Prelude

import qualified Fission.Web.API.Host.Types as Web

class Monad m => MonadReflectiveServer m where
  getHost :: m Web.Host
