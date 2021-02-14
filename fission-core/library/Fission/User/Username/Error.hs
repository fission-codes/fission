-- | Username errors
module Fission.User.Username.Error (Invalid (..)) where

import           Fission.Prelude

data Invalid = Invalid
  deriving ( Show
           , Eq
           , Exception
           )

instance Display Invalid where
  display Invalid = "Invalid username -- must be alphanumeric separated with hyphens and not blocklisted"
