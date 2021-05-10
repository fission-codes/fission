module Fission.Web.Server.Handler.App.Index (index) where

import           Database.Persist.Sql
import           RIO.Map                                as Map
import           Servant

import           Fission.Prelude

import           Fission.URL.Types

import           Fission.Web.API.App.Index.Types

import qualified Fission.Web.Server.App                 as App
import qualified Fission.Web.Server.App.Domain          as App.Domain
import           Fission.Web.Server.Authorization.Types
import           Fission.Web.Server.Models
import           Fission.Web.Server.MonadDB

index :: (MonadDB t m, App.Retriever t, App.Domain.Retriever t) => ServerT Index m
index Authorization {about = Entity userId _} = runDB do
  apps        <- App.ownedBy userId
  appXDomains <- forM apps findDomains
  return $ Map.fromList appXDomains

  where
    findDomains :: App.Domain.Retriever m => Entity App -> m (Natural, Payload)
    findDomains (Entity appId App {appInsertedAt, appModifiedAt}) = do
      appDomains <- App.Domain.allForApp appId

      let
        natKey :: Natural
        natKey = fromIntegral $ fromSqlKey appId

        payload :: Payload
        payload =
          Payload
            { urls       = toURL <$> appDomains
            , insertedAt = appInsertedAt
            , modifiedAt = appModifiedAt
            }

      return (natKey, payload)

    toURL :: Entity AppDomain -> URL
    toURL (Entity _ AppDomain {..}) = URL appDomainDomainName appDomainSubdomain
