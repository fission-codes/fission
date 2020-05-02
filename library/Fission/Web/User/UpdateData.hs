module Fission.Web.User.UpdateData
  ( API
  , server
  ) where

import           Database.Esqueleto
import           Network.IPFS.CID.Types
import           Servant

import           Fission.Prelude
import           Fission.Authorization

import           Fission.Web.Error as Web.Error
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
  => Authorization
  -> ServerT API m
server Authorization {about = Entity userID _} newCID = do
  Web.Error.ensure =<< runDBNow (User.setData userID newCID)
  return NoContent
  -- FIXME pin the file!!

-- type StreamingAPI
--   = Summary ""
--   :> Description ""
--   :> Capture "newCID" CID
--   :> (Stream 'PATCH 204)  '[PlainText, OctetStream, JSON] NoContent

-- streaming ::
