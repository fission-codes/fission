module Fission.App.Domain.Dissociator.Class
  ( Dissociator (..)
  , Errors'
  ) where

import           Database.Esqueleto

import           Fission.Error            as Error
import           Fission.Models
import           Fission.Ownership
import           Fission.Prelude
import           Fission.URL

import           Fission.App.Domain.Error as Domain
import qualified Fission.App.Retriever    as App

type Errors' = OpenUnion
  '[ Domain.NotRegisteredToApp
   , UserNotAuthorized App
   , NotFound          App
   ]

class Monad m => Dissociator m where
  dissociate ::
       UserId
    -> AppId
    -> DomainName
    -> Maybe Subdomain
    -> UTCTime
    -> m (Either Errors' ())

instance MonadIO m => Dissociator (Transaction m) where
  dissociate userId appId domainName maySubdomain now =
    App.byId userId appId >>= \case
      Left err ->
        return $ Error.relaxedLeft err

      Right app ->
        case app `isOwnedBy` userId of
          False ->
            return . Error.openLeft $ UserNotAuthorized @App userId

          True -> do
            insert_ DissociateAppDomainEvent
              { dissociateAppDomainEventAppId      = appId
              , dissociateAppDomainEventDomainName = domainName
              , dissociateAppDomainEventSubdomain  = maySubdomain
              , dissociateAppDomainEventInsertedAt = now
              }

            howMany <- deleteCount $ from \appDomain ->
              where_ $   appDomain ^. AppDomainAppId      ==. val appId
                     &&. appDomain ^. AppDomainDomainName ==. val domainName
                     &&. appDomain ^. AppDomainSubdomain  ==. val maySubdomain

            return case howMany of
              0 -> Error.openLeft $ Domain.NotRegisteredToApp appId domainName maySubdomain
              _ -> ok
