module Fission.Storage.Query.Class (MonadDBQuery (..)) where

import           Network.IPFS.CID.Types
import           Database.Esqueleto hiding ((=.), update)

import           Fission.Models
import           Fission.Prelude hiding (Value)
import           Fission.Types

-- getByGeneric :: (SqlExpr (Entity model) -> SqlExpr (Value Bool)) -> Transaction m [Entity model]
getByGeneric whereClause = select <| from \model -> do
      where_ (whereClause model)
      return model

getOneByGeneric whereClause = select <| from \model -> do
      where_ (whereClause model)
      limit 1
      return model

class MonadDB m => MonadDBQuery model m where -- Note that this is Monad not MonadDB
  getBy :: (SqlExpr (Entity model) -> SqlExpr (Value Bool)) -> Transaction m [Entity model]
  getOneBy :: (SqlExpr (Entity model) -> SqlExpr (Value Bool)) -> Transaction m [Entity model]

instance MonadDBQuery UserCid Fission where
  getBy = getByGeneric
  getOneBy = getOneByGeneric

instance MonadDBQuery User Fission where
  getBy = getByGeneric
  getOneBy = getOneByGeneric

instance MonadDBQuery HerokuAddOn Fission where
  getBy = getByGeneric
  getOneBy = getOneByGeneric