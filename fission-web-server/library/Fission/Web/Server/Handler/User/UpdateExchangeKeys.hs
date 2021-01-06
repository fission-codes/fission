module Fission.Web.Server.Handler.User.UpdateExchangeKeys
  ( handler
  , addKey
  , removeKey
  ) where

import           RIO.NonEmpty
import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.User.ExchangeKey.Types as API
import qualified Fission.Web.API.User.ExchangeKey.Types as API.ExchangeKey

import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.Error               as Web.Error
import qualified Fission.Web.Server.User                as User

handler :: (MonadTime m, MonadLogger m, MonadThrow m, User.Modifier m) => ServerT API.ExchangeKeys m
handler = addKey :<|> removeKey

addKey :: (MonadTime m, MonadLogger m, MonadThrow m, User.Modifier m) => ServerT API.ExchangeKey.Add m
addKey key Authorization {about = Entity userId _} = do
  now  <- currentTime
  keys <- Web.Error.ensureM $ User.addExchangeKey userId key now

  case nonEmpty keys of
    Nothing      -> return [key]
    Just allKeys -> return allKeys

removeKey :: (MonadTime m, MonadLogger m, MonadThrow m, User.Modifier m) => ServerT API.ExchangeKey.Remove m
removeKey key Authorization {about = Entity userId _} = do
  now <- currentTime
  Web.Error.ensureM $ User.removeExchangeKey userId key now
