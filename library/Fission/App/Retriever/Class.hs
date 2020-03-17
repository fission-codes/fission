module Fission.App.Retriever.Class (Retriever (..)) where

import           Database.Esqueleto

import           Fission.Prelude hiding (on)
import           Fission.Models
import           Fission.Ownership

import           Fission.Error

type Errors = OpenUnion
  '[ ActionNotAuthorized App
   , NotFound            App
   ]

class Monad m => Retriever m where
  byId    :: UserId -> AppId -> m (Either Errors (Entity App))
  ownedBy :: UserId -> m [Entity App]

instance MonadIO m => Retriever (Transaction m) where
  byId userId appId =
    appId
      |> getEntity
      |> fmap \case
        Nothing  ->
          openLeft <| NotFound @App

        Just app ->
          if isOwnedBy userId app
            then Right app
            else openLeft <| ActionNotAuthorized @App userId

  ownedBy userId = do
    select <| from \app -> do
      where_ (app ^. AppOwnerId ==. val userId)
      return app
