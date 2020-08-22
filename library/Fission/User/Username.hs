-- | Top-level username module

module Fission.User.Username
  ( rawL
  , module Fission.User.Username.Types
  , module Fission.User.Username.Error
  , module Fission.User.Username.Validation
  ) where

import           Fission.Prelude

import           Fission.User.Username.Error
import           Fission.User.Username.Types
import           Fission.User.Username.Validation

rawL :: Functor f => (Text -> f Text) -> Username -> f Username
rawL = lens username \_ new -> Username new
