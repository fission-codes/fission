module Fission.Web.App.Update
  ( API
  , update
  ) where

import           Network.IPFS.CID.Types
import           Servant

import           Fission.Prelude
import           Fission.Authorization
import           Fission.Models (AppId)

import qualified Fission.App       as App
import           Fission.Web.Error as Web.Error

type API
  =  Summary "Set app content"
  :> Description "Update the content (CID) for an app"
  :> Capture "appID"  AppId
  :> Capture "newCID" CID
  :> PatchAccepted '[JSON] NoContent

update ::
  ( MonadLogger    m
  , MonadThrow     m
  , MonadTime      m
  , MonadDB      t m
  , App.Modifier t
  )
  => Authorization
  -> ServerT API m
update Authorization {about = Entity userId _} appId newCID = do
  Web.Error.ensure =<< runDBNow (App.updateCID userId appId newCID)
  return NoContent
