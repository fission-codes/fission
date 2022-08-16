module Fission.Web.Server.Handler.User.DID (handlerV_, handlerV3) where

import           Crypto.Key.Asymmetric.Public.Types
import           Crypto.Key.Asymmetric.Public.Oldstyle.Types

import           Servant
import           Servant.Server.Generic

import           Fission.Prelude
import           Fission.Challenge.Types
import qualified Fission.Web.API.User.DID.Types         as API
import qualified Fission.Web.API.User.DID.Types         as DID

import           Fission.Error
import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.Error               as Web.Error
import           Fission.Web.Server.Models
import qualified Fission.Web.Server.RecoveryChallenge   as RecoveryChallenge
import qualified Fission.Web.Server.User                as User

handlerV3 ::
  ( MonadTime m
  , MonadLogger m
  , MonadThrow m
  , User.Modifier m
  , User.Retriever m
  , RecoveryChallenge.Retriever m
  , RecoveryChallenge.Destroyer m
  )
  => DID.RoutesV3 (AsServerT m)
handlerV3 =
  DID.RoutesV3 { setAuthenticated = handlerAuthenticated :: (MonadTime m, MonadLogger m, MonadThrow m, User.Modifier m) => ServerT ("did" :> API.SetAuthenticatedV3) m
               , setViaChallenge  = handlerViaChallenge
               }

handlerV_ ::
  ( MonadTime m
  , MonadLogger m
  , MonadThrow m
  , User.Modifier m
  , User.Retriever m
  , RecoveryChallenge.Retriever m
  , RecoveryChallenge.Destroyer m
  )
  => DID.RoutesV_ (AsServerT m)
handlerV_ =
  DID.RoutesV_ { setAuthenticated = handlerAuthenticated' :: (MonadTime m, MonadLogger m, MonadThrow m, User.Modifier m) => ServerT ("did" :> API.SetAuthenticatedV1) m
               , setViaChallenge  = handlerViaChallenge'
               }
  where
    handlerAuthenticated' (Oldstyle pk) = handlerAuthenticated pk
    handlerViaChallenge' (Oldstyle pk)  = handlerViaChallenge pk


handlerAuthenticated :: (MonadTime m, MonadLogger m, MonadThrow m, User.Modifier m) => Public -> Authorization -> m NoContent
handlerAuthenticated pk Authorization {about = Entity userID _} = do
  now <- currentTime
  Web.Error.ensureM $ User.updatePublicKey userID pk now
  return NoContent


handlerViaChallenge :: (MonadTime m, MonadLogger m, MonadThrow m, User.Retriever m,
 RecoveryChallenge.Retriever m, User.Modifier m,
 RecoveryChallenge.Destroyer m) =>
  Public
  -> User.Username
  -> Challenge
  -> m NoContent
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
