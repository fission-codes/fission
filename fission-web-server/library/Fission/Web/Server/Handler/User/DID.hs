module Fission.Web.Server.Handler.User.DID (handler) where

import           Servant
import           Servant.Server.Generic

import           Fission.Prelude

import           Fission.Error
import qualified Fission.User.DID.Types                 as DID

import qualified Fission.Web.API.User.DID.Types         as API
import qualified Fission.Web.API.User.DID.Types         as DID

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
  => DID.Routes (AsServerT m)
handler =
  DID.Routes { setAuthenticated = handlerAuthenticated
             , setViaChallenge  = handlerViaChallenge
             }
  where
    handlerAuthenticated :: (MonadTime m, MonadLogger m, MonadThrow m, User.Modifier m) => ServerT ("did" :> API.SetAuthenticated) m
    handlerAuthenticated pk Authorization {about = Entity userID _} = do
      now <- currentTime
      Web.Error.ensureM $ User.updateDID userID (DID.Key pk) now
      return NoContent

    handlerViaChallenge pk username challenge = do
      now <- currentTime

      Entity userId _ <- Web.Error.ensureMaybe noSuchUsername =<< User.getByUsername username
      challengeStored <- Web.Error.ensureM $ RecoveryChallenge.retrieve userId now

      when (challengeStored /= challenge)
        (Web.Error.throw (NotFound @UserRecoveryChallenge))

      Web.Error.ensureM $ User.updateDID userId (DID.Key pk) now

      RecoveryChallenge.destroyForUser userId

      return NoContent

    noSuchUsername =
      err422 { errBody = "Couldn't find a user with such username" }
