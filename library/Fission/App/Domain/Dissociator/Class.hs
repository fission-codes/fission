module Fission.App.Domain.Dissociator.Class
  ( Dissociator (..)
  , Errors
  ) where

import           Database.Esqueleto

import           Fission.Prelude
import           Fission.Models
import           Fission.Models.Error
import           Fission.Ownership
import           Fission.URL

import qualified Fission.Error as Error

import qualified Fission.App.Domain.Dissociate.Error as DissociateDomain
import qualified Fission.App.Retriever.Class         as App

type Errors = OpenUnion
  '[ DissociateDomain.NotRegisteredToApp
   , ActionNotAuthorized App
   , NotFound            App
   ]

class Monad m => Dissociator m where
  dissociate ::
       UserId
    -> AppId
    -> DomainName
    -> Maybe Subdomain
    -> UTCTime
    -> m (Either Errors ())

instance MonadIO m => Dissociator (Transaction m) where
  dissociate userId appId domainName maySubdomain now =
    App.byId userId appId >>= \case
      Left err ->
        return . Left <| relaxOpenUnion err

      Right app ->
        case isOwnedBy userId app of
          False ->
            return . Error.openLeft <| ActionNotAuthorized @App userId

          True -> do
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
              _ -> ok
