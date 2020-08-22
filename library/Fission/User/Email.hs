module Fission.User.Email
  ( rawL
  , module Fission.User.Email.Types
  ) where

import           Fission.Prelude

import           Fission.User.Email.Types

rawL :: Functor f => (Text -> f Text) -> Email -> f Email
rawL = lens unEmail \_ new -> Email new
