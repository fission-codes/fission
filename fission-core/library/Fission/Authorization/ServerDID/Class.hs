module Fission.Authorization.ServerDID.Class (ServerDID (..)) where

import           Fission.Prelude

import           Web.DID.Types

class Monad m => ServerDID m where
  getServerDID :: m DID
