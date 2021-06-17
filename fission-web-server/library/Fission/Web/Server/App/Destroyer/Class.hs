module Fission.Web.Server.App.Destroyer.Class
  ( Destroyer (..)
  , Errors'
  ) where

import           Database.Esqueleto
import           Servant.Server

import           Fission.Prelude

import           Fission.Error
import           Fission.URL

import qualified Fission.Web.Server.AWS.Zone.Types                  as AWS
import           Fission.Web.Server.Error.ActionNotAuthorized.Types
import           Fission.Web.Server.Models
import           Fission.Web.Server.MonadDB
import           Fission.Web.Server.Ownership

import qualified Fission.Web.Server.App.Domain.Retriever            as AppDomain
import qualified Fission.Web.Server.App.Retriever                   as App

type Errors' = OpenUnion
  '[ NotFound            App
   , ActionNotAuthorized App

   , NotFound            AppDomain
   , ActionNotAuthorized AppDomain

   , NotFound            URL
   , ActionNotAuthorized URL
   , InvalidURL

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
