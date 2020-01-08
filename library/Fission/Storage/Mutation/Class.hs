module Fission.Storage.Mutation.Class (MonadDBMutation (..)) where

import           Network.IPFS.CID.Types
import           Database.Esqueleto hiding ((=.), update)

import           Fission.Models
import           Fission.Prelude hiding (Value)
import           Fission.Types

deleteWhereGeneric whereClause = delete <| from \model -> where_ (whereClause model)

class MonadDB m => MonadDBMutation model m where -- Note that this is Monad not MonadDB
  deleteWhere :: (SqlExpr (Entity model) -> SqlExpr (Value Bool)) -> Transaction m ()

instance MonadDBMutation User Fission where
  deleteWhere = deleteWhereGeneric



