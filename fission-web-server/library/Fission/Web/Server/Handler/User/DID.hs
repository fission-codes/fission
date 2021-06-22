module Fission.Web.Server.Handler.User.DID (handler) where

import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.User.DID.Types         as API

import           Fission.Error
import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.Error               as Web.Error
import           Fission.Web.Server.Models
import qualified Fission.Web.Server.RecoveryChallenge   as RecoveryChallenge
import qualified Fission.Web.Server.User                as User


handler ::
  ( MonadTime m
  , MonadLogger m
  , MonadThrow m
  , User.Modifier m
  , User.Retriever m
  , RecoveryChallenge.Retriever m
  , RecoveryChallenge.Destroyer m
  )
  => ServerT API.DID m
handler =
       handlerAuthenticated
  :<|> handlerViaChallenge


handlerAuthenticated :: (MonadTime m, MonadLogger m, MonadThrow m, User.Modifier m) => ServerT ("did" :> API.SetAuthenticated) m
handlerAuthenticated pk Authorization {about = Entity userID _} = do
  now <- currentTime
  Web.Error.ensureM $ User.updatePublicKey userID pk now
  return NoContent


handlerViaChallenge ::
  ( MonadTime m
  , MonadLogger m
  , MonadThrow m
  , User.Modifier m
  , User.Retriever m
  , RecoveryChallenge.Retriever m
  , RecoveryChallenge.Destroyer m
  )
  => ServerT ("did" :> API.SetViaChallenge) m

handlerViaChallenge pk username challenge = do
  now <- currentTime

  Entity userId _ <- Web.Error.ensureMaybe noSuchUsername =<< User.getByUsername username
  challengeStored <- Web.Error.ensureM $ RecoveryChallenge.retrieve userId now

  when (challengeStored /= challenge)
    (Web.Error.throw (NotFound @UserRecoveryChallenge))

  Web.Error.ensureM $ User.updatePublicKey userId pk now

  RecoveryChallenge.destroyForUser userId

  return NoContent

  where
    noSuchUsername =
      err422 { errBody = "Couldn't find a user with such username" }
