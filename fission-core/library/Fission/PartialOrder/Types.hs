module Fission.PartialOrder.Types (Relationship (..)) where

import           Fission.Prelude

data Relationship
  = Ancestor
  | Descendant
  | Sibling
  | Equal
  deriving (Eq, Show)
