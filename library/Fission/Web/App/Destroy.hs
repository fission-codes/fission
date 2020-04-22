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
import           Fission.Web.Error.Class

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
  ( MonadTime       m
  , MonadThrow      m
  , MonadDB       t m
  , App.Destroyer t
  )
  => Authorization
  -> ServerT API m
server auth = destroyByURL auth
         :<|> destroyById  auth

destroyByURL ::
  ( MonadTime       m
  , MonadThrow      m
  , MonadDB       t m
  , App.Destroyer t
  )
  => Authorization
  -> ServerT ByURLAPI m
destroyByURL Authorization {about = Entity userId _} URL {..} =
  runDBNow (App.destroyByURL userId domainName subdomain) >>= \case
    Right () -> return NoContent
    Left err -> throwM $ toServerError err

destroyById ::
  ( MonadTime       m
  , MonadThrow      m
  , MonadDB       t m
  , App.Destroyer t
  )
  => Authorization
  -> ServerT ByIdAPI m
destroyById Authorization {about = Entity userId _} appId =
  runDBNow (App.destroy userId appId) >>= \case
    Right () -> return NoContent
    Left err -> throwM $ toServerError err
