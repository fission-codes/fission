module Fission.Web.App.Update
  ( API
  , update
  ) where

import           Database.Esqueleto (Entity (..))
import           Network.IPFS.CID.Types
import           Servant

import           Fission.Prelude
import           Fission.Models (User, AppId)

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
  => Entity User
  -> ServerT API m
update (Entity userId _) appId newCID = do
  Web.Error.ensure =<< runDBNow (App.updateCID userId appId newCID)
  return NoContent
