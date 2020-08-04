module Fission.App.Retriever.Class (Retriever (..)) where

import           Database.Esqueleto hiding ((<&>))

import           Fission.Models
import           Fission.Ownership
import           Fission.Prelude    hiding (on)

import           Fission.Error      as Error
import           Fission.URL

type Errors' = OpenUnion
  '[ ActionNotAuthorized App
   , NotFound            App
   ]

class Monad m => Retriever m where
  byId    :: UserId -> AppId -> m (Either Errors' (Entity App))
  byURL   :: UserId -> URL -> m (Either Errors' (Entity App))
  ownedBy :: UserId -> m [Entity App]

instance MonadIO m => Retriever (Transaction m) where
  byId userId appId =
    getEntity appId <&> \case
      Nothing  ->
        openLeft $ NotFound @App

      Just app ->
        if isOwnedBy userId app
          then Right app
          else openLeft $ ActionNotAuthorized @App userId

  ownedBy userId =
    select $ from \app -> do
      where_ (app ^. AppOwnerId ==. val userId)
      return app

  byURL userId URL {..} = do
    mayAppDomain <- select $ from \appDomain -> do
      where_ $ appDomain ^. AppDomainDomainName ==. val domainName
           &&. appDomain ^. AppDomainSubdomain  ==. val subdomain
      limit 1
      return appDomain

    case mayAppDomain of
      [] ->
        return . Error.openLeft $ NotFound @App

      (Entity _ AppDomain {appDomainAppId} : _) -> do
        mayApp <- select $ from \app -> do
          where_ $ app ^. AppId ==. val appDomainAppId
          limit 1
          return app

        return case mayApp of
          [] ->
            Error.openLeft $ NotFound @App

          (app : _) ->
            if isOwnedBy userId app
              then Right app
              else Error.openLeft $ ActionNotAuthorized @App userId
