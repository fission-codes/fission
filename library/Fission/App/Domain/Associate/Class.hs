module Fission.App.Domain.Associate.Class
  ( Associate (..)
  , Errors
  , associateDefault
  ) where

import           Database.Esqueleto (Entity (..), insert_, insertUnique)

import           Fission.Prelude
import           Fission.Models
import           Fission.URL

import           Fission.Authorization.Error
import           Fission.Ownership

import qualified Fission.App.Retriever.Class as App
import qualified Fission.App.Domain.Associate.Error as AppDomain

import qualified Fission.Error as Error
import           Fission.Models.Error

type Errors = OpenUnion
  '[ AppDomain.AlreadyExists
   , Unauthorized
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
    App.byId appId >>= \case
      Left NotFound ->
        return <| Error.openLeft Unauthorized

      Right (Entity _ app) ->
        case isOwnedBy userId app of
          False ->
            return <| Error.openLeft Unauthorized

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
                Just _  -> return ()

associateDefault ::
  ( MonadIO   m
  , Associate m
  )
  => UserId
  -> AppId
  -> UTCTime
  -> m (Either Errors Subdomain)
associateDefault userId appId now = do
  subdomain <- liftIO (generate arbitrary)

  associate userId appId defaultDomainName (Just subdomain) now
    <&> fmap \_ -> subdomain

defaultDomainName :: DomainName
defaultDomainName = DomainName "fission.app"
