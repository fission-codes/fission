module Fission.Web.Server.Handler.DNS (hanlder) where

import           Database.Esqueleto
import           Servant

import           Network.IPFS.CID.Types

import           Fission.Prelude

import           Fission.Authorization
import           Fission.Web.Server.Models

import           Fission.URL               as URL

import qualified Fission.User.Modifier     as User

-- Deprecated! Works the "old" way with direct access to username.fission.name,
-- WITHOUT the `files` prefix
handler ::
  ( MonadTime     m
  , MonadThrow    m
  , MonadLogger   m
  , User.Modifier m
  )
  => Authorization
  -> ServerT API m
handler Authorization {about = Entity userID User {userUsername}} cid = do
  now <- currentTime
  Web.Err.ensureM $ User.setData userID cid now
  return . DomainName $ textDisplay userUsername <> ".fission.name"
