module Fission.Storage.Query
  ( one
  , many

  , wherever
  , oneWhere
  , manyWhere
  , deleteWhere

  , module Database.Esqueleto
  ) where

import Database.Esqueleto
import Fission.Prelude


-- BASE


{-| Select the first item.
-}
one = from .> selectFirst


{-| Select multiple items.
-}
many = from .> select



-- WHERE QUERYING


{-| Simplified `where_`.
-}
wherever fn a = where_ (fn a) >> return a


{-| Select the first item restricted by the given WHERE clause.

    oneWhere (\user -> user ^. username ==. "fission")

-}
oneWhere = wherever .> one


{-| Select multiple items restricted by the given WHERE clause.

    manyWhere (\user -> user ^. active ==. True)

-}
manyWhere = wherever .> many


{-| Delete all items matching the given WHERE clause.

    deleteWhere (\user -> user ?. email ==. just "steven@fission.codes")

-}
deleteWhere = wherever .> delete



-- CREATE
--
-- See `Database.Esqueleto.insert`



-- UPDATE
--
-- See `Database.Esqueleto.update`
