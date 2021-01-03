module Fission.Authorization.ServerDID.Class
  ( ServerDID          (..)
--   , PublicizeServerDID (..)
  ) where

import           Fission.Prelude

import           Fission.User.DID.Types

class Monad m => ServerDID m where
  getServerDID :: m DID

-- FIXME move to server
--
-- class Monad m => PublicizeServerDID m where
  --publicize :: m (Either ServerError ())
