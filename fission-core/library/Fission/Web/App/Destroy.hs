module Fission.Web.App.Destroy
  ( API
  , ByURLAPI
  , ByIdAPI
  , server
  , destroyById
  , destroyByURL
  ) where

import           Database.Esqueleto
import           Servant

import           Fission.Prelude
import           Fission.Authorization

import           Fission.Models
import           Fission.URL.Types

import qualified Fission.App.Destroyer.Class as App
import qualified Fission.Web.Error as Web.Error

type API = ByURLAPI :<|> ByIdAPI

type ByURLAPI
  =  Summary "Destroy app by URL"
  :> Description "Destroy app by any associated URL"
  :> "associated"
  :> Capture "url" URL
  :> DeleteNoContent '[JSON] NoContent

type ByIdAPI
  =  Summary "Destroy app by ID"
  :> Description "Destroy app by its ID"
  :> Capture "appId" AppId
  :> DeleteNoContent '[JSON] NoContent

server ::
  ( MonadTime     m
  , MonadThrow    m
  , MonadLogger   m
  , App.Destroyer m
  )
  => Authorization
  -> ServerT API m
server auth = destroyByURL auth
         :<|> destroyById  auth

destroyByURL ::
  ( MonadTime     m
  , MonadThrow    m
  , MonadLogger   m
  , App.Destroyer m
  )
  => Authorization
  -> ServerT ByURLAPI m
destroyByURL Authorization {about = Entity userId _} URL {..} = do
  now <- currentTime
  Web.Error.ensureM $ App.destroyByURL userId domainName subdomain now
  return NoContent

destroyById ::
  ( MonadTime     m
  , MonadThrow    m
  , MonadLogger   m
  , App.Destroyer m
  )
  => Authorization
  -> ServerT ByIdAPI m
destroyById Authorization {about = Entity userId _} appId = do
  now <- currentTime
  Web.Error.ensureM $ App.destroy userId appId now
  return NoContent
