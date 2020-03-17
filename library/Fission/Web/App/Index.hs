module Fission.Web.App.Index
  ( API
  , index
  ) where

import           Database.Esqueleto (Entity (..))
import           Servant

import           Fission.Prelude
import           Fission.Models

import Fission.URL.Types

import qualified Fission.App.Domain as App.Domain

type API
  = Get '[JSON] [URL]

index :: (MonadDB t m, App.Domain.Retriever t) => Entity User -> ServerT API m
index (Entity userId _) = runDB do
  appDomains <- App.Domain.allForOwner userId
  return (toURL <$> appDomains)
  where
    toURL (Entity _ AppDomain {..}) = URL appDomainDomainName appDomainSubdomain
