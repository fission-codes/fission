module Fission.Web.User.DataRoot.Update
  ( API
  , server
  ) where

import           Database.Esqueleto
import           Network.IPFS.CID.Types
import           Servant

import           Fission.Prelude

import           Fission.Config.Types

import qualified Fission.Authorization                      as Authorization
import           Fission.Authorization.Session.Trans
import           Fission.Web.Auth.Token.UCAN.Resource.Types

import           Fission.Authorization.Session.Class

import qualified Fission.User                               as User
import           Fission.Web.Error                          as Web.Error

import           Fission.WNFS.Privilege.Types               as WNFS

type API
  =  Summary "Update data root"
  :> Description "Set/update user's file system content"
  :> Capture "username" User.Username
  :> Capture "newCID"   CID
  :> PatchNoContent '[PlainText, OctetStream, JSON] NoContent

server ::
  ( MonadLogger   m
  , MonadThrow    m
  , MonadSTM      m
  , MonadTime     m
  , User.Modifier m
  )
  => Authorization.Session
  -> ServerT API m
server session username newCID =
  runSessionT session do
    now <- currentTime
    Web.Error.ensureM $ User.setData username newCID now
    return NoContent
