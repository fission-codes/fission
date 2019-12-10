module Fission.Storage.Database.Query
  ( one
  , many
  -- Where
  , wherever
  , oneWhere
  , manyWhere
  , deleteWhere
  -- Reexport
  , module Database.Esqueleto
  ) where

import Database.Esqueleto as Esqueleto
import qualified Database.Esqueleto.Internal.Sql as Esqueleto
import Fission.Prelude hiding (many)
import Fission.Storage.Database.Class
import Fission.Storage.Database.Types


-- BASE


{-| Select the first item.
-}
one
  :: ( Esqueleto.From a
     , Esqueleto.SqlSelect b c
     , MonadDatabase m c
     )
  => (a -> Query b)
  -> Transaction m (Maybe c)
one fn = fn
  |> from
  |> bind (\result -> limit 1 >> return result)
  |> select
  |> fmap headMaybe


{-| Select multiple items.
-}
many = from .> select



-- WHERE QUERYING


{-| Simplified `where_`.
-}
wherever :: Esqueleto.From a => (a -> Query b)
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
