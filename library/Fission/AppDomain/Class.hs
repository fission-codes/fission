module Fission.AppDomain.Class where

import           Fission.Prelude

import           Fission.Models
import           Fission.URL

import qualified Fission.Error as Error

class Monad m => Creator m where
  create :: AppId -> DomainId -> Maybe Subdomain -> UTCTime -> m (Maybe AppDomainId)

instance MonadIO m => Creator (Transaction m) where
  create appId domainId maySubdomain now =
    AppDomain
      { appDomainAppId      = userId
      , appDomainDomainId   = domainId
      , appDomainSubdomain  = maySubdomain
      , appDomainInsertedAt = now
      , appDomainModifiedAt = now
      }
      |> insertUnique
      |> fmap (Error.fromMaybe' AppDomain.AlreadyExists)
