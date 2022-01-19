module Fission.Web.Server.Handler.App.Destroy (handler) where

import           Database.Esqueleto.Legacy

import           Servant
import           Servant.Server.Generic

import           Fission.Prelude

import           Fission.URL.Types

import qualified Fission.Web.API.App.Destroy.Types      as App.Destroy

import qualified Fission.Web.Server.App.Destroyer.Class as App
import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.Error               as Web.Error

handler :: (MonadTime m, MonadThrow m, MonadLogger m, App.Destroyer m) => App.Destroy.Routes (AsServerT m)
handler = App.Destroy.Routes {..}
  where
    byURL URL {..} Authorization {about = Entity userId _} = do
      now <- currentTime
      Web.Error.ensureM $ App.destroyByURL userId domainName subdomain now
      return NoContent

    byID appId Authorization {about = Entity userId _} = do
      now <- currentTime
      Web.Error.ensureM $ App.destroy userId (toSqlKey $ fromIntegral appId) now
      return NoContent
