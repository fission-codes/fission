-- | Username errors

module Fission.User.Username.Error (Invalid (..)) where

import           Servant.Server

import           Fission.Prelude
import           Fission.Web.Error

data Invalid = Invalid
  deriving ( Show
           , Eq
           , Exception
           )

instance Display Invalid where
  display Invalid = "Invalid username -- must be alphanumeric separated with hyphens and not blocklisted"

instance ToServerError Invalid where
  toServerError Invalid =
    err422 { errBody = displayLazyBS Invalid }
