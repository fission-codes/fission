module Fission.Storage.Query (is', getOne) where

import RIO
import RIO.List (headMaybe)

import Database.Selda

is' :: Row s t -> Selector t Bool -> Col s Bool
is' row prop = row ! prop .== true

getOne :: Functor f => f [a] -> f (Maybe a)
getOne = fmap headMaybe
