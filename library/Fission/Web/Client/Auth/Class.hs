module Fission.Web.Client.Auth.Class (HasWebAuth (..)) where

import           Fission.Prelude

import           Fission.Web.Auth.Token.JWT
import           Fission.User.DID

class Monad m => HasWebAuth m where
  ucanJWT :: m JWT
  rawDID  :: m DID
