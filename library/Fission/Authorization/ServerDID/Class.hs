module Fission.Authorization.ServerDID.Class (ServerDID (..)) where

import Servant.Server

import Fission.Prelude

import Fission.User.DID.Types

class Monad m => ServerDID m where
  getServerDID :: m DID
  publicize    :: m (Either ServerError ())
