module Fission.PartialOrder
  ( isEncompassedBy
  , module Fission.PartialOrder.Class
  , module Fission.PartialOrder.Types
  ) where

import           RIO

import           Fission.PartialOrder.Class
import           Fission.PartialOrder.Types

isEncompassedBy :: PartialOrder a => a -> a -> Bool
x `isEncompassedBy` y =
  case relationship x y of
    Equal      -> True
    Descendant -> True
    _          -> False
