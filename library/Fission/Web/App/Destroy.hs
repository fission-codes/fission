module Fission.Web.App.Destroy
  ( API
  , destroy
  ) where

import           Database.Esqueleto
import           Servant

import           Fission.Prelude
import           Fission.Models
import           Fission.URL.Types

import qualified Fission.App.Destroyer.Class as App
import           Fission.Web.Error.Class

type API
  =  Capture "url" URL
  :> DeleteNoContent '[JSON] NoContent

destroy ::
  ( MonadTime       m
  , MonadThrow      m
  , MonadDB       t m
  , App.Destroyer t
  )
  => Entity User
  -> ServerT API m
destroy (Entity userId _) URL {..} =
  runDBNow (App.destroyByURL userId domainName subdomain) >>= \case
    Right () -> return NoContent
    Left err -> throwM <| toServerError err
