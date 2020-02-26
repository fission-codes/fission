module Fission.App.Retriever.Class (Retriever (..)) where

import           Database.Esqueleto

import           Fission.Prelude
import           Fission.Models

import           Fission.Models.Error

class Monad m => Retriever m where
  byId :: AppId -> m (Either (NotFound App) (Entity App))

instance MonadIO m => Retriever (Transaction m) where
  byId appId =
    appId
      |> getEntity
      |> fmap \case
        Nothing  -> Left (NotFound @App)
        Just app -> Right app
