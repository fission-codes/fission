module Fission.Storage.Query.Class (MonadDBQuery (..)) where

import           Network.IPFS.CID.Types
import           Database.Esqueleto hiding ((=.), update)

import           Fission.Models
import           Fission.Prelude hiding (Value)
import           Fission.Types

getByGeneric ::
  ( PersistEntity model
  , BackendCompatible SqlBackend (PersistEntityBackend model)
  , MonadIO m
  )
  => (SqlExpr (Entity model) -> SqlExpr (Value Bool))
  -> Transaction m [Entity model]
getByGeneric whereClause = select <| from \model -> do
      where_ (whereClause model)
      return model

-- getOneByGeneric whereClause = select <| from \model -> do
--       where_ (whereClause model)
--       limit 1
--       return model

class (MonadDB m, PersistEntity model, BackendCompatible SqlBackend (PersistEntityBackend model)) => MonadDBQuery model m where
    getBy :: (SqlExpr (Entity model) -> SqlExpr (Value Bool)) -> Transaction m [Entity model]
    getBy = getByGeneric
  -- getOneBy :: (SqlExpr (Entity model) -> SqlExpr (Value Bool)) -> Transaction m [Entity model]

instance MonadDBQuery UserCid Fission where
  getBy = getByGeneric
  -- getOneBy = getOneByGeneric

instance MonadDBQuery User Fission where
  getBy = getByGeneric
  -- getOneBy = getOneByGeneric

instance MonadDBQuery HerokuAddOn Fission where
  getBy = getByGeneric
  -- getOneBy = getOneByGeneric