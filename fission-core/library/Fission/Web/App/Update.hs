module Fission.Web.App.Update
  ( API
  , update
  ) where

import           Servant

import           Network.IPFS.CID.Types

import           Fission.Prelude

-- import           Fission.Authorization
-- import           Fission.Web.Auth.Token.UCAN.Resource.Types
import qualified Fission.Authorization  as Authorization

import qualified Fission.App            as App
import           Fission.Web.Error      as Web.Error

import           Fission.URL.Types


type API
  =  Summary     "Set app content"
  :> Description "Update the content (CID) for an app"
  :> Capture     "App URL" URL
  :> Capture     "New CID" CID
  :> QueryParam  "copy-data" Bool
  :> PatchAccepted '[JSON] NoContent

update ::
  ( MonadLogger  m
  , MonadThrow   m
  , MonadTime    m
  , App.Modifier m
  )
  => Authorization.Session
  -> ServerT API m
update Authorization.Session {} url newCID copyDataFlag = do
-- update Authorization {about = Entity userId _} url newCID copyDataFlag = do
  let userId = undefined -- FIXME
  now <- currentTime
  Web.Error.ensureM $ App.setCID userId url newCID copyFiles now
  return NoContent
  where
    copyFiles :: Bool
    copyFiles = maybe True identity copyDataFlag
