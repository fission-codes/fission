module Fission.App.Domain.Dissociate.Class
  ( Dissociate (..)
  , Errors
  ) where

import           Database.Esqueleto
import qualified Database.Persist as P

import           Fission.Prelude
import           Fission.Models
import           Fission.URL

import qualified Fission.Error as Error

import qualified Fission.App.Domain.Dissociate.Error as DissociateDomain

type Errors = OpenUnion
  '[ DissociateDomain.NotRegisteredToApp
   ]

class Monad m => Dissociate m where
  dissociate :: AppId -> DomainName -> Maybe Subdomain -> UTCTime -> m (Either Errors ())
  dissociateAllByDomainName :: AppId -> DomainName -> UTCTime -> m (Either Errors ())

instance MonadIO m => Dissociate (Transaction m) where
  dissociate appId domainName maySubdomain now = do
    insert_ DissociateAppDomainEvent
      { dissociateAppDomainEventAppId      = appId
      , dissociateAppDomainEventDomainName = domainName
      , dissociateAppDomainEventSubdomain  = maySubdomain
      , dissociateAppDomainEventInsertedAt = now
      }

    howMany <- deleteCount <| from \appDomain ->
      where_ <|  appDomain ^. AppDomainAppId      ==. val appId
             &&. appDomain ^. AppDomainDomainName ==. val domainName
             &&. appDomain ^. AppDomainSubdomain  ==. val maySubdomain

    return case howMany of
      0 -> Error.openLeft <| DissociateDomain.NotRegisteredToApp appId domainName maySubdomain
      _ -> Right ()

  dissociateAllByDomainName appId domainName now = do
    appDomains <- select <| from \appDomain -> do
      where_ <|  appDomain ^. AppDomainAppId      ==. val appId
             &&. appDomain ^. AppDomainDomainName ==. val domainName
      return appDomain

    case appDomains of
      [] ->
        return . Error.openLeft <| DissociateDomain.NotRegisteredToApp appId domainName Nothing

      _ -> do
        forM_ appDomains \(Entity appDomainId AppDomain {..}) -> do
          P.delete appDomainId
          insert_ DissociateAppDomainEvent
            { dissociateAppDomainEventAppId      = appDomainAppId
            , dissociateAppDomainEventDomainName = appDomainDomainName
            , dissociateAppDomainEventSubdomain  = appDomainSubdomain
            , dissociateAppDomainEventInsertedAt = now
            }

        return <| Right ()
