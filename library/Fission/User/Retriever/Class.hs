module Fission.User.Retriever.Class (Retriever (..)) where

import           Database.Esqueleto hiding ((=.), update)
import qualified Database.Persist as P

import           Fission.Prelude
import           Fission.Models

import           Fission.User.Email.Types
import           Fission.User.DID.Types
import           Fission.User.Username.Types

class Monad m => Retriever m where
  getByUsername      :: Username      -> m (Maybe (Entity User))
  getByDid           :: DID           -> m (Maybe (Entity User))
  getByHerokuAddOnId :: HerokuAddOnId -> m (Maybe (Entity User))
  getByEmail         :: Email         -> m (Maybe (Entity User))

instance MonadIO m => Retriever (Transaction m) where
  getByUsername username = selectFirst
    [ UserUsername P.==. username
    , UserActive   P.==. True
    ] []

  getByDid did = selectFirst
    [ UserDid      P.==. Just did
    , UserActive   P.==. True
    ] []

  getByHerokuAddOnId addOnId = selectFirst
    [ UserHerokuAddOnId P.==. Just addOnId
    , UserActive        P.==. True
    ] []

  getByEmail email = selectFirst
    [ UserEmail  P.==. Just email
    , UserActive P.==. True
    ] []
