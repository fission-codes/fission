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

import qualified Fission.Authorization                       as Auth
import           Fission.Web.Auth.Token.UCAN.Privilege.Types

import           Fission.Models
import           Fission.URL.Types

import qualified Fission.App.Destroyer.Class                 as App
import qualified Fission.Web.Error                           as Web.Error

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
  => Auth.Session
  -> ServerT API m
server auth = destroyByURL auth
         :<|> destroyById  auth

-- FIXME 1. ensure that the user has rights to this app!
-- FIXME 2. change relationship to "source" and "alias" (follower) apps
destroyByURL ::
  ( MonadTime     m
  , MonadThrow    m
  , MonadLogger   m
  , App.Destroyer m
  )
  => Auth.Session
  -> ServerT ByURLAPI m
destroyByURL = undefined -- FIXME!
-- destroyByURL Auth.Session {about = Entity userId _} URL {..} = do
--   now <- currentTime
--   Web.Error.ensureM $ App.destroyByURL userId domainName subdomain now
--   return NoContent

-- FIXME 1. ensure that the user has rights to this app!
-- FIXME 2. change relationship to "source" and "alias" (follower) apps
destroyById ::
  ( MonadTime     m
  , MonadThrow    m
  , MonadLogger   m
  , App.Destroyer m
  )
  => Auth.Session
  -> ServerT ByIdAPI m
destroyById = undefined -- FIXME
--destroyById Auth.Session {about = Entity userId _} appId = do
--  now <- currentTime
--  Web.Error.ensureM $ App.destroy userId appId now
--  return NoContent
