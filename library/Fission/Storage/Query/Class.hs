module Fission.Storage.Query.Class (MonadDBQuery (..)) where

import           Network.IPFS.CID.Types
import           Database.Esqueleto hiding ((=.), update)

import           Fission.Models
import           Fission.Prelude hiding (Value)
import           Fission.Types
import qualified Database.Persist as P

getByGeneric ::
  ( PersistEntity model
  , PersistRecordBackend model SqlBackend
  , MonadDB m
  )
  => (SqlExpr (Entity model) -> SqlExpr (Value Bool))
  -> Transaction m [Entity model]
getByGeneric whereClause = select <| from \model -> do
      where_ (whereClause model)
      return model

getOneByGeneric ::
  ( PersistEntity model
  , PersistRecordBackend model SqlBackend
  , MonadDB m
  )
  => [P.Filter model]
  -> Transaction m (Maybe (Entity model))
getOneByGeneric filters = selectFirst filters []

class (MonadDB m, PersistEntity model, PersistRecordBackend model SqlBackend) => MonadDBQuery model m where
    getBy :: (SqlExpr (Entity model) -> SqlExpr (Value Bool)) -> Transaction m [Entity model]
    getOneBy :: [P.Filter model] -> Transaction m (Maybe (Entity model))

--  TODO how to make instances go away
instance MonadDBQuery UserCid Fission where
  getBy = getByGeneric
  getOneBy = getOneByGeneric

instance MonadDBQuery User Fission where
  getBy = getByGeneric
  getOneBy = getOneByGeneric

instance MonadDBQuery HerokuAddOn Fission where
  getBy = getByGeneric
  getOneBy = getOneByGeneric