module Fission.Key.Asymmetric.Algorithm.Error (Invalid (..)) where

import           Fission.Prelude

data Invalid = Invalid
  deriving (Show, Eq, Exception)

instance Display Invalid where
  display _ = "Asymmetric.Algorithm.Invalid"
