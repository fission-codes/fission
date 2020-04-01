module Fission.User.Retriever.Class (Retriever (..)) where

import           Database.Persist

import           Fission.Prelude
import qualified Fission.Key as Key
import           Fission.Models

import           Fission.User.Email.Types
import           Fission.User.Username.Types

class Monad m => Retriever m where
  getByUsername      :: Username      -> m (Maybe (Entity User))
  getByPublicKey     :: Key.Public    -> m (Maybe (Entity User))
  getByHerokuAddOnId :: HerokuAddOnId -> m (Maybe (Entity User))
  getByEmail         :: Email         -> m (Maybe (Entity User))

instance MonadIO m => Retriever (Transaction m) where
  getByUsername username = selectFirst
    [ UserUsername ==. username
    , UserActive   ==. True
    ] []

  getByPublicKey pk = selectFirst
    [ UserPublicKey ==. Just pk
    , UserActive    ==. True
    ] []

  getByHerokuAddOnId addOnId = selectFirst
    [ UserHerokuAddOnId ==. Just addOnId
    , UserActive        ==. True
    ] []

  getByEmail email = selectFirst
    [ UserEmail  ==. Just email
    , UserActive ==. True
    ] []
