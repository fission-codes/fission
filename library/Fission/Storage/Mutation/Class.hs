module Fission.Storage.Mutation.Class (MonadDBMutation (..)) where

import           Network.IPFS.CID.Types
import           Database.Esqueleto hiding ((=.), update)

import           Fission.Models
import           Fission.Prelude hiding (Value)
import           Fission.Types

deleteWhereGeneric ::
  ( PersistEntity model
  , PersistRecordBackend model SqlBackend
  , MonadDB m
  )
  => (SqlExpr (Entity model) -> SqlExpr (Value Bool))
  -> Transaction m ()
deleteWhereGeneric whereClause = delete <| from \model -> where_ (whereClause model)
-- Could also be
-- deleteWhereGeneric = Esqueleto.deleteWhere

class (MonadDB m, PersistEntity model, PersistRecordBackend model SqlBackend) => MonadDBMutation model m where
  deleteWhere :: (SqlExpr (Entity model) -> SqlExpr (Value Bool)) -> Transaction m ()

instance MonadDBMutation User Fission where
  deleteWhere = deleteWhereGeneric

instance MonadDBMutation User Fission where
  deleteWhere = deleteWhereGeneric


