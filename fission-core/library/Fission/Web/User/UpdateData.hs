module Fission.Web.User.UpdateData
  ( API
  , server
  ) where

import           Database.Esqueleto
import           Servant

import           Fission.Prelude
import           Fission.Authorization

import           Fission.Web.Error as Web.Error
import qualified Fission.User as User

import           Network.IPFS.CID.Types


type API
  =  Summary "Update data root"
  :> Description "Set/update currently authenticated user's file system content"
  :> Capture "newCID" CID
  :> PatchNoContent '[PlainText, OctetStream, JSON] NoContent

server ::
  ( MonadLogger     m
  , MonadThrow      m
  , MonadTime       m
  , User.Modifier   m
  )
  => Authorization
  -> ServerT API m
server Authorization {about = Entity userID _} newCID = do
  now <- currentTime
  Web.Error.ensureM $ User.setData userID newCID now
  return NoContent
