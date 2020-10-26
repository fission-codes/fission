module Fission.Web.App.Index
  ( API
  , index
  ) where

import           RIO.Map                                    as Map
import           Servant

import qualified Fission.Authorization                      as Authorization
import           Fission.Prelude
import           Fission.Web.Auth.Token.UCAN.Resource.Types

import           Fission.Models
import           Fission.URL.Types

import qualified Fission.App                                as App
import qualified Fission.App.Domain                         as App.Domain

type API
  =  Summary "App index"
  :> Description "A list of all of your apps and their associated domain names"
  :> Get '[JSON] (Map AppId [URL])

index ::
  ( MonadDB              t m
  , App.Retriever        t
  , App.Domain.Retriever t
  )
  => Authorization.Session
  -> ServerT API m
index Authorization.Session {} = runDB do
-- index Authorization {about = Entity userId _} = runDB do
  let userId = undefined -- FIXME

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
