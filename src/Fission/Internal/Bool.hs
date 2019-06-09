module Fission.Internal.Bool (anyX) where

import RIO

anyX :: [a -> Bool] -> a -> Bool
anyX preds value = any (== True) (preds <*> [value])
