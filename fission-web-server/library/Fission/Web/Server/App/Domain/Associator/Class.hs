module Fission.Web.Server.App.Domain.Associator.Class
  ( Associator (..)
  , Errors'
  ) where

import           Database.Esqueleto.Legacy                          (Checkmark (..),
                                                                     insertUnique,
                                                                     insert_)

import           Fission.Prelude

import           Fission.Error                                      as Error
import           Fission.URL

import qualified Fission.Web.Server.App.Domain.Error                as AppDomain
import qualified Fission.Web.Server.App.Retriever                   as App
import           Fission.Web.Server.Error.ActionNotAuthorized.Types
import           Fission.Web.Server.Models
import           Fission.Web.Server.MonadDB
import           Fission.Web.Server.Ownership

type Errors' = OpenUnion
  '[ AppDomain.AlreadyAssociated
   , ActionNotAuthorized App
   , NotFound            App
   ]

class Monad m => Associator m where
  associate ::
       UserId
    -> AppId
    -> Checkmark
    -> DomainName
    -> Maybe Subdomain
    -> UTCTime
    -> m (Either Errors' ())

instance MonadIO m => Associator (Transaction m) where
  associate userId appId isPrimary domainName maySubdomain now =
    App.byId userId appId >>= \case
      Left err ->
        return $ relaxedLeft err

      Right (Entity _ app) ->
        case isOwnedBy userId app of
          False ->
            return . Error.openLeft $ ActionNotAuthorized @App userId

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
              , appDomainIsPrimary  = isPrimary
              , appDomainInsertedAt = now
              }
              |> insertUnique
              |> fmap \case
                Nothing -> Error.openLeft $ AppDomain.AlreadyAssociated appId domainName maySubdomain
                Just _  -> ok
