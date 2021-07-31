module Fission.Web.Server.Handler.App.Update (update, updateStreaming) where

import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.App.Update.Streaming.Types as API.App
import qualified Fission.Web.API.App.Update.Types           as API.App

import qualified Fission.Web.Server.App                     as App
import           Fission.Web.Server.Authorization.Types
import           Fission.Web.Server.Error                   as Web.Error

update :: (MonadLogger m, MonadThrow m, MonadTime m, App.Modifier m) => ServerT API.App.Update m
update url newCID copyDataFlag Authorization {about = Entity userId _} = do
  now <- currentTime
  Web.Error.ensureM $ App.setCID userId url newCID copyFiles now
  return ()
  where
    copyFiles :: Bool
    copyFiles = maybe True identity copyDataFlag

updateStreaming :: (MonadIO m, MonadTime m, App.Modifier m) => ServerT API.App.StreamingUpdate m
updateStreaming  url newCID Authorization {about = Entity userId _} = do
  now <- currentTime
  App.setCIDStreaming userId url newCID now
