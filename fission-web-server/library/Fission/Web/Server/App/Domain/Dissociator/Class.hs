module Fission.Web.Server.App.Domain.Dissociator.Class
  ( Dissociator (..)
  , Errors'
  ) where

import           Database.Esqueleto

import           Fission.Prelude

import           Fission.Error                       as Error
import           Fission.URL

import           Fission.Web.Server.Models
import           Fission.Web.Server.MonadDB
import           Fission.Web.Server.Ownership

import           Fission.Web.Server.App.Domain.Error as Domain
import qualified Fission.Web.Server.App.Retriever    as App

type Errors' = OpenUnion
  '[ Domain.NotRegisteredToApp
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
    -> m (Either Errors' ())

instance MonadIO m => Dissociator (Transaction m) where
  dissociate userId appId domainName maySubdomain now =
    App.byId userId appId >>= \case
      Left err ->
        return $ Error.relaxedLeft err

      Right app ->
        case isOwnedBy userId app of
          False ->
            return . Error.openLeft $ ActionNotAuthorized @App undefined -- FIXME DID -- userId

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
