module Fission.App.Domain.Dissociate.Class where

import           Database.Esqueleto

import           Fission.Prelude
import           Fission.Models
import           Fission.URL

import qualified Fission.Error as Error

import           Fission.URL.DomainName.Error
import           Fission.URL.Subdomain.Error

import qualified Fission.App.Domain.Dissociate.Error as DissociateDomain

type Errors = OpenUnion
  '[ DissociateDomain.NotRegisteredToApp
   ]

class Monad m => Dissociate m where
  dissociate ::
       AppId
    -> DomainName
    -> Maybe Subdomain
    -> UTCTime
    -> m (Either Errors ())

  dissociateAllByDomainName ::
       AppId
    -> DomainName
    -> UTCTime
    -> m (Either Errors ())

instance MonadIO m => Dissociate (Transaction m) where
  dissociate appId domainName maySubdomain now = do
    insert_ DissociateAppDomainEvent
      { dissociateAppDomainEventAppId      = appId
      , dissociateAppDomainEventDomainName = domainName
      , dissociateAppDomainEventSubdomain  = maySubdomain
      , dissociateAppDomainEventInsertedAt = now
      }

    count <- deleteCount <| from \appDomain ->
      where_ <|  appDomain ^. AppDomainAppId      ==. val appId
             &&. appDomain ^. AppDomainDomainName ==. val domainName
             &&. appDomain ^. AppDomainSubdomain  ==. val maySubdomain

    return if count == 0
             then Error.openLeft <| DissociateDomain.NotRegisteredToApp appId domainName maySubdomain
             else Right ()

  dissociateAllByDomainName appId domainName now = do
    appDomains <- select <| from \appDomain ->
      where_ <|  appDomain ^. AppDomainAppId      ==. val appId
             &&. appDomain ^. AppDomainDomainName ==. val domainName

    if null appDomains
      then
        return . Error.openLeft <| DissociateDomain.NotRegisteredToApp appId domainName Nothing

      else
        Right <$> forM_ appDomains \AppDomain {..} ->
          insert_ DissociateAppDomainEvent
            { dissociateAppDomainEventAppId      = appDomainAppId
            , dissociateAppDomainEventDomainName = appDomainDomainName
            , dissociateAppDomainEventSubdomain  = appDomainSubdomain
            , dissociateAppDomainEventInsertedAt = now
            }
