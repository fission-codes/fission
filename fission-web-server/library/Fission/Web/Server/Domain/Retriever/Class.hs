module Fission.Web.Server.Domain.Retriever.Class (Retriever (..)) where

import qualified Database.Persist                        as Persist

import           Fission.Prelude

import           Fission.Error

import           Fission.Web.Server.Models
import           Fission.Web.Server.URL.DomainName.Types

class Monad m => Retriever m where
  getByDomainName :: DomainName -> m (Either (NotFound Domain) Domain)

instance MonadIO m => Retriever (Transaction m) where
  getByDomainName domainName = do
    Persist.get (DomainKey domainName) <&> \case
      Nothing     -> Left $ NotFound @Domain
      Just domain -> Right domain
