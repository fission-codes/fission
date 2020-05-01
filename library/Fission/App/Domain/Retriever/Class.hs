module Fission.App.Domain.Retriever.Class
  ( Retriever (..)
  , Errors
  ) where

import qualified Database.Persist   as P
import           Database.Esqueleto

import           Fission.Prelude hiding (on)
import           Fission.Error
import           Fission.Models
import           Fission.URL

type Errors = OpenUnion
  '[ NotFound AppDomain
   ]

class Monad m => Retriever m where
  allForOwner         :: UserId -> m [Entity AppDomain]
  allForApp           :: AppId  -> m [Entity AppDomain]
  allSiblingsByDomain :: DomainName -> Maybe Subdomain -> m (Either Errors [Entity AppDomain])

instance MonadIO m => Retriever (Transaction m) where
  allForApp appId = P.selectList [AppDomainAppId P.==. appId] []

  allSiblingsByDomain domainName maySubdomain = do
    mayAppDomain <- P.selectFirst
      [ AppDomainDomainName P.==. domainName
      , AppDomainSubdomain  P.==. maySubdomain
      ]
      []

    case mayAppDomain of
      Nothing ->
        return . openLeft <| NotFound @AppDomain

      Just (Entity _ AppDomain {appDomainAppId}) ->
        Right <$> P.selectList [AppDomainAppId P.==. appDomainAppId] []

  allForOwner ownerId = do
    select <| from \(app `InnerJoin` appDomain) -> do
      on <| app       ^. AppOwnerId ==. val ownerId
        &&. appDomain ^. AppDomainAppId ==. app ^. AppId

      return appDomain
