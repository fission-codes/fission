module Fission.App.Name.Error (Invalid (..)) where

import           Fission.Prelude

data Invalid = Invalid
  deriving (Eq, Show, Exception)

instance Display Invalid where
  display Invalid = "Invalid App.Name -- must be alphanumeric separated with hyphens and not blocklisted"
