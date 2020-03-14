module Fission.App.Domain.Associate.Class
  ( Associate (..)
  , Errors
  ) where

import           Database.Esqueleto (Entity (..), insert_, insertUnique)

import           Fission.Prelude
import           Fission.Models
import           Fission.Models.Error
import           Fission.Ownership
import           Fission.URL

import qualified Fission.App.Retriever.Class        as App
import qualified Fission.App.Domain.Associate.Error as AppDomain

import qualified Fission.Error as Error

type Errors = OpenUnion
  '[ AppDomain.AlreadyExists
   , ActionNotAuthorized App
   , NotFound            App
   ]

class Monad m => Associate m where
  associate ::
       UserId
    -> AppId
    -> DomainName
    -> Maybe Subdomain
    -> UTCTime
    -> m (Either Errors ())

instance MonadIO m => Associate (Transaction m) where
  associate userId appId domainName maySubdomain now =
    App.byId userId appId >>= \case
      Left err ->
        return . Left <| relaxOpenUnion err

      Right (Entity _ app) ->
        case isOwnedBy userId app of
          False ->
            return . Error.openLeft <| ActionNotAuthorized @App userId

          True -> do
            insert_ AssociateAppDomainEvent
              { associateAppDomainEventAppId      = appId
              , associateAppDomainEventDomainName = domainName
              , associateAppDomainEventSubdomain  = maySubdomain
              , associateAppDomainEventInsertedAt = now
              }

            AppDomain
              { appDomainAppId      = appId
              , appDomainDomainName = domainName
              , appDomainSubdomain  = maySubdomain
              , appDomainInsertedAt = now
              }
              |> insertUnique
              |> fmap \case
                Nothing -> Error.openLeft <| AppDomain.AlreadyExists appId domainName maySubdomain
                Just _  -> ok
