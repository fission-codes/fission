module Fission.Storage.Query
  ( is'
  , all'
  , findOne
  , oneEq
  ) where

import Database.Selda as Selda
import Database.Selda.Backend

import Fission.Prelude

is' :: Row s t -> Selector t Bool -> Col s Bool
is' row prop = row ! prop .== true

all' :: Row s t -> [Row s t -> Col s Bool] -> Col s Bool
all' row = foldr (\prop acc -> acc .&& prop row) true

findOne
  :: ( MonadSelda m
     , Selda.Result (OuterCols a)
     )
  => Query (Inner (Backend m)) a
  -> m (Maybe (Res (OuterCols a)))
findOne = fmap headMaybe . query . limit 0 1

oneEq
  :: ( MonadSelda m
     , Relational t
     , SqlType r
     )
  => Table t
  -> Selector t r
  -> r
  -> m (Maybe t)
oneEq tbl selector target = findOne <| select tbl `suchThat` (selector `is` target)
