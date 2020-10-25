module Fission.PartialOrder.Types (Relationship (..)) where

import           RIO

data Relationship
  = Ancestor
  | Descendant
  | Sibling
  | Equal
  deriving (Eq, Show)
