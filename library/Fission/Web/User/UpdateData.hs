module Fission.Web.User.UpdateData
  ( API
  , server
  ) where

import           Database.Esqueleto
import           Network.IPFS.CID.Types
import           Servant

import           Fission.Prelude
import           Fission.Web.Error as Web.Error
import           Fission.Models
import qualified Fission.User as User

type API
  =  Summary "Update data root"
  :> Description "Set/update currently authenticated user's file system content"
  :> Capture "newCID" CID
  :> PatchNoContent '[PlainText, OctetStream, JSON] NoContent

server ::
  ( MonadLogger     m
  , MonadThrow      m
  , MonadTime       m
  , MonadDB       t m
  , User.Modifier t
  )
  => Entity User
  -> ServerT API m
server (Entity userID _) newCID = do
  Web.Error.ensure =<< runDBNow (User.setData userID newCID)
  return NoContent
