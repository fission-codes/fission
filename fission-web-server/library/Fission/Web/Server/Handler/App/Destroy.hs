module Fission.Web.Server.Handler.App.Destroy
  ( handler
  , destroyById
  , destroyByURL
  ) where

import           Database.Esqueleto
import           Servant

import           Fission.Prelude

import           Fission.URL.Types

import qualified Fission.Web.API.App.Destroy.Types      as API.App
import qualified Fission.Web.API.App.Destroy.Types      as API.App.Destroy

import qualified Fission.Web.Server.App.Destroyer.Class as App
import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.Error               as Web.Error

handler :: (MonadTime m, MonadThrow m, MonadLogger m, App.Destroyer m) => ServerT API.App.Destroy m
handler = destroyByURL :<|> destroyById

destroyByURL :: (MonadTime m, MonadThrow m, MonadLogger m, App.Destroyer m) => ServerT API.App.Destroy.ByURL m
destroyByURL URL {..} Authorization {about = Entity userId _} = do
  now <- currentTime
  Web.Error.ensureM $ App.destroyByURL userId domainName subdomain now
  return NoContent

destroyById :: (MonadTime m, MonadThrow m, MonadLogger m, App.Destroyer m) => ServerT API.App.Destroy.ById m
destroyById appId Authorization {about = Entity userId _} = do
  now <- currentTime
  Web.Error.ensureM $ App.destroy userId (toSqlKey $ fromIntegral appId) now
  return NoContent
