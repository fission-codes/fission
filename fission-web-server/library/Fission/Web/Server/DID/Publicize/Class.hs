module Fission.Web.Server.DID.Publicize.Class (Publicize (..)) where

import           Servant.Server

import           Fission.Prelude

class Monad m => Publicize m where
  publicize :: m (Either ServerError ())
