module Crypto.Key.Asymmetric.Algorithm.Error (Invalid (..)) where

import           RIO

data Invalid = Invalid
  deriving (Show, Eq, Exception)

instance Display Invalid where
  display _ = "Asymmetric.Algorithm.Invalid"
