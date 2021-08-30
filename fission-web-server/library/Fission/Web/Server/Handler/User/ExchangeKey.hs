module Fission.Web.Server.Handler.User.ExchangeKey (handler) where

import           RIO.NonEmpty
import           Servant.Server.Generic

import           Fission.Prelude

import qualified Fission.Web.API.User.ExchangeKey.Types as ExchangeKey

import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.Error               as Web.Error
import qualified Fission.Web.Server.User                as User

handler ::
  ( MonadTime m
  , MonadLogger m
  , MonadThrow m
  , User.Modifier m
  )
  => ExchangeKey.Routes (AsServerT m)
handler =
  ExchangeKey.Routes {..}
  where
    add key Authorization {about = Entity userId _} = do
      now  <- currentTime
      keys <- Web.Error.ensureM $ User.addExchangeKey userId key now

      case nonEmpty keys of
        Nothing      -> return [key]
        Just allKeys -> return allKeys

    remove key Authorization {about = Entity userId _} = do
      now <- currentTime
      Web.Error.ensureM $ User.removeExchangeKey userId key now
