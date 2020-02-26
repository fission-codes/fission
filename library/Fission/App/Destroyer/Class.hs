module Fission.App.Destroyer.Class
  ( Destroyer (..)
  , Errors
  ) where

import           Database.Esqueleto

import           Fission.Prelude
import           Fission.Models
import           Fission.URL

import           Fission.Models.Error
import           Fission.Error

import qualified Fission.App.Domain.Retriever.Class as AppDomain

type Errors = OpenUnion
  '[ NotFound            App
   , ActionNotAuthorized App
   ]

class Monad m => Destroyer m where
  destroy      :: UserId -> AppId -> UTCTime -> m (Either Errors ())
  destroyByURL :: UserId -> DomainName -> Maybe Subdomain -> UTCTime -> m (Either Errors ())

instance AppDomain.Retriever m => Destroyer (Transaction m) where
  destroy userId appId now = do
    AppDomain.allForApp appId >>= \case
      Left err -> return <| Left err
      Right appDomains ->
        guardOwnedBy userId app \_ -> do
        -- FIXME move to App.Retriever.allDomainsFor appId

        putMany (toEvent now <$> appDomains)
        deleteCascade appId
        return ok

  destroyByURL userId domainName maySubdomain now = do
  -- FIXME move to App.Retriever.allDomainsByURL domainName URL
    appDomains <- select <| from \appDomain -> do
      where_ <| appDomain ^. AppDomainDomainName ==. val domainName
            &&. appDomain ^. AppDomainSubdomain  ==. val maySubdomain
      return appDomain

    case appDomains of
      (Entity appId AppDomain {..} : _) -> do
        putMany (toEvent now <$> appDomains)
        deleteCascade appId
        return ok

      _ ->
        return ok

toEvent :: UTCTime -> Entity AppDomain -> DissociateAppDomainEvent
toEvent now (Entity _ AppDomain {..}) =
  DissociateAppDomainEvent
    { dissociateAppDomainEventAppId      = appDomainAppId
    , dissociateAppDomainEventDomainName = appDomainDomainName
    , dissociateAppDomainEventSubdomain  = appDomainSubdomain
    , dissociateAppDomainEventInsertedAt = now
    }

-- common :: UserId -> Entity AppDomain ->
