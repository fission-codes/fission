module Fission.Storage.Database.Query
  ( one
  , many
  -- Where
  , wherever
  , oneWhere
  , manyWhere
  , deleteWhere
  -- Reexport
  , module Esqueleto
  ) where

import Database.Esqueleto as Esqueleto hiding (deleteWhere)
import qualified Database.Esqueleto as Esqueleto (insert)
import qualified Database.Esqueleto.Internal.Sql as Esqueleto
import Fission.Prelude hiding (many)
import Fission.Storage.Database.Class
import Fission.Storage.Database.Types as Database


-- BASE


{-| Select the first item.
-}
one
  :: ( Esqueleto.From a
     , Esqueleto.SqlSelect b c
     )
  => (a -> Query b)
  -> Database.Transaction m (Maybe c)
one fn = fn
  |> from
  |> bind (\result -> limit 1 >> return result)
  |> select
  |> fmap headMaybe


{-| Select multiple items.
-}
many
  :: ( Esqueleto.From a
     , Esqueleto.SqlSelect b c
     )
  => (a -> Query b)
  -> Database.Transaction m [c]
many = from .> select



-- WHERE QUERYING


{-| Same as `where_`, but returns a Query holding the given value.
-}
wherever
  :: Esqueleto.SqlExpr (Esqueleto.Value Bool)
  -> a
  -> Query a
wherever fn a = where_ fn >> return a


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
deleteWhere
  :: Esqueleto.From ()
  => Esqueleto.SqlExpr (Esqueleto.Value Bool)
  -> Database.Transaction m ()
deleteWhere = wherever .> from .> delete



-- CREATE
--
-- See `Database.Esqueleto.insert`



-- UPDATE
--
-- See `Database.Esqueleto.update`
