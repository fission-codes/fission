module Fission.App.Destroyer.Class
  ( Destroyer (..)
  , Errors
  ) where

import           Database.Esqueleto

import           Fission.Prelude
import           Fission.Models
import           Fission.Ownership
import           Fission.URL

import           Fission.Models.Error
import           Fission.Error

import qualified Fission.App.Domain.Retriever.Class as AppDomain
import qualified Fission.App.Retriever.Class as App

type Errors = OpenUnion
  '[ NotFound            App
   , ActionNotAuthorized App
   , NotFound            AppDomain
   ]

class Monad m => Destroyer m where
  destroy      :: UserId -> AppId -> UTCTime -> m (Either Errors ())
  destroyByURL :: UserId -> DomainName -> Maybe Subdomain -> UTCTime -> m (Either Errors ())

instance (MonadIO m) => Destroyer (Transaction m) where
  destroy userId appId now = do
    appDomains <- AppDomain.allForApp appId
    destroyAssociated userId appId appDomains now

  destroyByURL userId domainName maySubdomain now =
    AppDomain.allSiblingsByDomain domainName maySubdomain >>= \case
      Left  errs ->
        return . Left <| relaxOpenUnion errs

      Right [] ->
        return . openLeft <| NotFound @AppDomain

      Right appDomains@(Entity _ AppDomain {appDomainAppId} : _) ->
        destroyAssociated userId appDomainAppId appDomains now

destroyAssociated ::
  MonadIO m
  => UserId
  -> AppId
  -> [Entity AppDomain]
  -> UTCTime
  -> Transaction m (Either Errors ())
destroyAssociated userId appId appDomains now =
  App.byId appId >>= \case
    Left err ->
      return <| openLeft err

    Right (Entity _ app) ->
      if isOwnedBy userId app
        then do
          putMany (toEvent now <$> appDomains)
          deleteCascade appId
          return ok
        else
          return . openLeft <| ActionNotAuthorized @App userId

toEvent :: UTCTime -> Entity AppDomain -> DissociateAppDomainEvent
toEvent now (Entity _ AppDomain {..}) =
  DissociateAppDomainEvent
    { dissociateAppDomainEventAppId      = appDomainAppId
    , dissociateAppDomainEventDomainName = appDomainDomainName
    , dissociateAppDomainEventSubdomain  = appDomainSubdomain
    , dissociateAppDomainEventInsertedAt = now
    }
