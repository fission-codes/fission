module Fission.Authorization.ServerDID.Class
  ( ServerDID          (..)
  , PublicizeServerDID (..)
  ) where

import Servant.Server

import Fission.Prelude

import Fission.User.DID.Types

class Monad m => ServerDID m where
  getServerDID :: m DID

class Monad m => PublicizeServerDID m where
  publicize :: m (Either ServerError ())
