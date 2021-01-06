module Fission.Web.Server.Handler.User.UpdateExchangeKeys
  ( handler
  , addKey
  , removeKey
  ) where

import qualified Crypto.PubKey.RSA         as RSA
import           Servant

import           Fission.Prelude

import           Fission.Authorization

import qualified Fission.Web.Server.Error  as Web.Error
import           Fission.Web.Server.Models
import qualified Fission.Web.Server.User   as User

handler ::
  ( MonadTime     m
  , MonadLogger   m
  , MonadThrow    m
  , User.Modifier m
  )
  => Authorization
  -> ServerT API m
handler Authorization {about = Entity userId _} = addKey userId :<|> removeKey userId

addKey ::
  ( MonadTime     m
  , MonadLogger   m
  , MonadThrow    m
  , User.Modifier m
  )
  => UserId
  -> ServerT AddAPI m
addKey userId key = do
  now <- currentTime
  Web.Error.ensureM $ User.addExchangeKey userId key now

removeKey ::
  ( MonadTime     m
  , MonadLogger   m
  , MonadThrow    m
  , User.Modifier m
  )
  => UserId
  -> ServerT RemoveAPI m
removeKey userId key = do
  now <- currentTime
  Web.Error.ensureM $ User.removeExchangeKey userId key now
