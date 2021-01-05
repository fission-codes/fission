module Fission.Web.Server.Handler.User.DataRoot.Update (handler) where

import           Database.Esqueleto
import           Network.IPFS.CID.Types
import           Servant

import           Fission.Prelude

import           Fission.Authorization

import           Fission.Web.Server.Error as Web.Error
import qualified Fission.Web.Server.User  as User

hanlder ::
  ( MonadLogger   m
  , MonadThrow    m
  , MonadTime     m
  , User.Modifier m
  )
  => Authorization
  -> ServerT API m
handler Authorization {about = Entity userID _} newCID = do
  now <- currentTime
  Web.Error.ensureM $ User.setData userID newCID now
  return NoContent
