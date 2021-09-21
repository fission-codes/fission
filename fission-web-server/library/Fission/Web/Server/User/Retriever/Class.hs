module Fission.Web.Server.User.Retriever.Class (Retriever (..)) where

import           Database.Persist

import           Fission.Prelude

import qualified Fission.Key                      as Key

import           Fission.User.DID.Types           as DID
import           Fission.User.Email.Types
import           Fission.User.Username.Types

import           Fission.Web.Server.Models
import           Fission.Web.Server.MonadDB.Types

class Monad m => Retriever m where
  getById            :: UserId        -> m (Maybe (Entity User))
  getByUsername      :: Username      -> m (Maybe (Entity User))
  getByDID           :: DID           -> m (Maybe (Entity User))
  getByHerokuAddOnId :: HerokuAddOnId -> m (Maybe (Entity User))
  getByEmail         :: Email         -> m (Maybe (Entity User))

instance MonadIO m => Retriever (Transaction m) where
  getById userId = selectFirst
    [ UserId     ==. userId
    , UserActive ==. True
    ] []

  getByUsername username = selectFirst
    [ UserUsername ==. username
    , UserActive   ==. True
    ] []

  getByDID (DID.Key pk) =
    selectFirst
      [ UserPublicKey ==. Just pk
      , UserActive    ==. True
      ] []

  getByDID (DID.ION ion) =
    selectFirst
      [ UserIonID  ==. Just ion
      , UserActive ==. True
      ] []

  getByHerokuAddOnId addOnId = selectFirst
    [ UserHerokuAddOnId ==. Just addOnId
    , UserActive        ==. True
    ] []

  getByEmail email = selectFirst
    [ UserEmail  ==. Just email
    , UserActive ==. True
    ] []
