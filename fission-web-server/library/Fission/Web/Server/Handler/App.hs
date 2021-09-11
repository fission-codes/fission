module Fission.Web.Server.Handler.App (handlerV2, handlerV_) where

import qualified Database.Persist.Sql                    as SQL
import           Network.IPFS.CID.Types
import qualified RIO.Map                                 as Map

import           Servant.Server.Generic

import           Fission.Prelude

import qualified Fission.App.Name                        as App
import qualified Fission.App.Name                        as App.Name
import           Fission.URL.Types

import           Fission.Web.API.App.Index.Payload.Types
import qualified Fission.Web.API.App.Types               as App

import           Fission.Web.Server.IPFS.DNSLink.Class   as DNSLink
import           Fission.Web.Server.Models
import           Fission.Web.Server.MonadDB

import qualified Fission.Web.Server.App                  as App
import qualified Fission.Web.Server.App.Content          as App.Content
import           Fission.Web.Server.App.Domain           as App.Domain

import           Fission.Web.Server.Authorization.Types
import           Fission.Web.Server.Error                as Web.Error
import qualified Fission.Web.Server.Handler.App.Destroy  as Destroy

handlerV2 ::
  ( App.Domain.Initializer  m
  , App.CRUD                m
  , App.Content.Initializer m
  , MonadTime               m
  , MonadLogger             m
  , MonadDNSLink            m
  , MonadDB               t m
  , App.Retriever         t
  , App.Domain.Retriever  t
  )
  => App.RoutesV2 (AsServerT m)
handlerV2 = App.RoutesV2 {index, create, update, destroy = genericServerT Destroy.handler}

handlerV_ ::
  ( App.Domain.Initializer  m
  , App.CRUD                m
  , App.Content.Initializer m
  , MonadTime               m
  , MonadLogger             m
  , MonadDNSLink            m
  , MonadDB               t m
  , App.Retriever         t
  , App.Domain.Retriever  t
  )
  => App.RoutesV_ (AsServerT m)
handlerV_ = App.RoutesV_ {index, create, update, destroy = genericServerT Destroy.handler}

index :: (MonadDB t m, App.Retriever t, Retriever t) => Authorization -> m (Map Natural Payload)
index Authorization {about = Entity userId _} =
  runDB do
    apps        <- App.ownedBy userId
    appXDomains <- forM apps findDomains
    return $ Map.fromList appXDomains

create ::
  ( MonadTime m
  , MonadLogger m
  , MonadThrow m
  , App.Creator m
  , App.Content.Initializer m
  , Initializer m
  )
  => Maybe App.Name
  -> Authorization
  -> m URL
create mayAppName Authorization {about = Entity userId _} = do
  now            <- currentTime
  (_, subdomain) <- Web.Error.ensureM $ App.createWithPlaceholder userId maySubdomain now
  defaultDomain  <- App.Domain.initial

  return URL
    { domainName = defaultDomain
    , subdomain  = Just subdomain
    }

  where
    maySubdomain :: Maybe Subdomain
    maySubdomain = App.Name.toSubdomain <$> mayAppName

update ::
  ( MonadTime m
  , MonadLogger m
  , MonadThrow m
  , App.Modifier m
  )
  => URL
  -> Network.IPFS.CID.Types.CID
  -> Maybe Bool
  -> Authorization
  -> m ()
update url newCID copyDataFlag Authorization {about = Entity userId _} = do
  now <- currentTime
  Web.Error.ensureM $ App.setCID userId url newCID copyFiles now
  return ()

  where
    copyFiles :: Bool
    copyFiles = maybe True identity copyDataFlag

findDomains :: App.Domain.Retriever m => Entity App -> m (Natural, Payload)
findDomains (Entity appId App {appInsertedAt, appModifiedAt}) = do
  appDomains <- App.Domain.allForApp appId

  let
    natKey :: Natural
    natKey = fromIntegral $ SQL.fromSqlKey appId

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
