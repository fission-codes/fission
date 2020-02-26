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
import           Fission.Models
import           Fission.URL.Types

import qualified Fission.App.Destroyer.Class as App
import           Fission.Web.Error.Class

type API = ByURLAPI :<|> ByIdAPI

type ByURLAPI
  =  "associated"
  :> Capture "url" URL
  :> DeleteNoContent '[JSON] NoContent

type ByIdAPI
  =  Capture "appId" AppId
  :> DeleteNoContent '[JSON] NoContent

server ::
  ( MonadTime       m
  , MonadThrow      m
  , MonadDB       t m
  , App.Destroyer t
  )
  => Entity User
  -> ServerT API m
server user = destroyByURL user :<|> destroyById user

destroyByURL ::
  ( MonadTime       m
  , MonadThrow      m
  , MonadDB       t m
  , App.Destroyer t
  )
  => Entity User
  -> ServerT ByURLAPI m
destroyByURL (Entity userId _) URL {..} =
  runDBNow (App.destroyByURL userId domainName subdomain) >>= \case
    Right () -> return NoContent
    Left err -> throwM <| toServerError err

destroyById ::
  ( MonadTime       m
  , MonadThrow      m
  , MonadDB       t m
  , App.Destroyer t
  )
  => Entity User
  -> ServerT ByIdAPI m
destroyById (Entity userId _) appId =
  runDBNow (App.destroy userId appId) >>= \case
    Right () -> return NoContent
    Left err -> throwM <| toServerError err
