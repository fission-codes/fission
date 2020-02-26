module Fission.App.Domain.Associate.Class
  ( Associate (..)
  , Errors
  , associateDefault
  ) where

import           Database.Esqueleto (insert_, insertUnique)

import           Fission.Prelude
import           Fission.Models
import           Fission.URL

import qualified Fission.Error                      as Error
import qualified Fission.App.Domain.Associate.Error as AppDomain

type Errors = OpenUnion
  '[ AppDomain.AlreadyExists
   ]

class Monad m => Associate m where
  associate ::
       AppId
    -> DomainName
    -> Maybe Subdomain
    -> UTCTime
    -> m (Either Errors ())

instance MonadIO m => Associate (Transaction m) where
  associate appId domainName maySubdomain now = do
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

associateDefault :: (MonadIO m, Associate m) => AppId -> UTCTime -> m (Either Errors Subdomain)
associateDefault appId now = do
  subdomain <- liftIO (generate arbitrary)

  associate appId defaultDomainName (Just subdomain) now
    <&> fmap \_ -> subdomain

defaultDomainName :: DomainName
defaultDomainName = DomainName "fission.app"
