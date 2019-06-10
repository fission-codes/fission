module Fission.Storage.Query
  ( is'
  , all'
  , getOne
  ) where

import RIO
import RIO.List (headMaybe)

import Database.Selda

is' :: Row s t -> Selector t Bool -> Col s Bool
is' row prop = row ! prop .== true

all' :: Row s t -> [(Row s t -> Col s Bool)] -> Col s Bool
all' row props = foldr (\prop acc -> acc .&& prop row) true props

getOne :: Functor f => f [a] -> f (Maybe a)
getOne = fmap headMaybe
