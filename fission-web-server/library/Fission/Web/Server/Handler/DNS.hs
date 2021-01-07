module Fission.Web.Server.Handler.DNS (handler) where

import           Database.Esqueleto
import           Servant

import           Fission.Prelude

import           Fission.URL                            as URL

import qualified Fission.Web.API.DNS.Types              as API

import           Fission.Web.Server.Authorization.Types
import           Fission.Web.Server.Error               as Web.Err
import           Fission.Web.Server.Models
import qualified Fission.Web.Server.User.Modifier       as User

-- Deprecated! Works the "old" way with direct access to username.fission.name,
-- WITHOUT the `files` prefix
handler ::
  ( MonadTime     m
  , MonadThrow    m
  , MonadLogger   m
  , User.Modifier m
  )
  => ServerT API.DNS m
handler cid Authorization {about = Entity userID User {userUsername}} = do
  now <- currentTime
  Web.Err.ensureM $ User.setData userID cid now
  return . DomainName $ textDisplay userUsername <> ".fission.name"
