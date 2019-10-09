module Fission.Internal.Bool
  ( anyX
  , truthy
  ) where

import RIO

anyX :: [a -> Bool] -> a -> Bool
anyX preds value = True `elem` (preds <*> [value])

truthy :: (Eq a, IsString a) => a -> Bool
truthy = anyX $ (==) <$> ["true", "yes", "1", "t", "on"]
