module Fission.App.Destroyer.Class
  ( Destroyer (..)
  , Errors'
  ) where

import           Database.Esqueleto
import           Servant.Server

import           Fission.Error
import           Fission.Models
import           Fission.Ownership
import           Fission.Prelude
import           Fission.URL

import qualified Fission.AWS.Zone.Types       as AWS

import qualified Fission.App.Domain.Retriever as AppDomain
import qualified Fission.App.Retriever        as App

type Errors' = OpenUnion
  '[ NotFound            App
   , ActionNotAuthorized App

   , NotFound            AppDomain
   , ActionNotAuthorized AppDomain

   , NotFound AWS.ZoneID

   , ServerError
   ]

class Monad m => Destroyer m where
  destroy ::
       UserId
    -> AppId
    -> UTCTime
    -> m (Either Errors' [URL])

  destroyByURL ::
       UserId
    -> DomainName
    -> Maybe Subdomain
    -> UTCTime
    -> m (Either Errors' [URL])

instance MonadIO m => Destroyer (Transaction m) where
  destroy userId appId now = do
    appDomains <- AppDomain.allForApp appId
    destroyAssociated userId appId appDomains now

  destroyByURL userId domainName maySubdomain now =
    AppDomain.allSiblingsByDomain domainName maySubdomain >>= \case
      Left  errs ->
        return $ relaxedLeft errs

      Right [] ->
        return . openLeft $ NotFound @AppDomain

      Right appDomains@(Entity _ AppDomain {appDomainAppId} : _) ->
        destroyAssociated userId appDomainAppId appDomains now

destroyAssociated ::
  MonadIO m
  => UserId
  -> AppId
  -> [Entity AppDomain]
  -> UTCTime
  -> Transaction m (Either Errors' [URL])
destroyAssociated userId appId appDomains now =
  App.byId userId appId >>= \case
    Left err ->
      return $ relaxedLeft err

    Right (Entity _ app) ->
      if isOwnedBy userId app
        then do
          putMany (toEvent now <$> appDomains)
          deleteCascade appId
          return $ Right (extractURL <$> appDomains)
        else
          return . openLeft $ ActionNotAuthorized @App userId

toEvent :: UTCTime -> Entity AppDomain -> DissociateAppDomainEvent
toEvent now (Entity _ AppDomain {..}) =
  DissociateAppDomainEvent
    { dissociateAppDomainEventAppId      = appDomainAppId
    , dissociateAppDomainEventDomainName = appDomainDomainName
    , dissociateAppDomainEventSubdomain  = appDomainSubdomain
    , dissociateAppDomainEventInsertedAt = now
    }

extractURL :: Entity AppDomain -> URL
extractURL (Entity _ AppDomain {..}) =
  URL
    { domainName = appDomainDomainName
    , subdomain  = appDomainSubdomain
    }
