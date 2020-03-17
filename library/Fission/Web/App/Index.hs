module Fission.Web.App.Index
  ( API
  , index
  ) where

import           Database.Esqueleto (Entity (..))
import           RIO.Map as Map
import           Servant

import           Fission.Prelude
import           Fission.Models
import           Fission.URL.Types

import qualified Fission.App        as App
import qualified Fission.App.Domain as App.Domain

type API
  = Get '[JSON] (Map AppId [URL])

index ::
  ( MonadDB              t m
  , App.Retriever        t
  , App.Domain.Retriever t
  )
  => Entity User
  -> ServerT API m
index (Entity userId _) = runDB do
  apps        <- App.ownedBy userId
  appXDomains <- forM apps findDomains
  return (Map.fromList appXDomains)
  where
    findDomains :: App.Domain.Retriever m => Entity App -> m (AppId, [URL])
    findDomains (Entity appId _) = do
      appDomains <- App.Domain.allForApp appId
      return (appId, toURL <$> appDomains)

    toURL :: Entity AppDomain -> URL
    toURL (Entity _ AppDomain {..}) = URL appDomainDomainName appDomainSubdomain
