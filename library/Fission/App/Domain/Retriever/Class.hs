module Fission.App.Domain.Retriever.Class
  ( Retriever (..)
  , Errors
  ) where

import           Database.Esqueleto
import qualified Database.Persist as P

import           Fission.Prelude
import           Fission.Models
import           Fission.URL

import           Fission.Models.Error
import           Fission.Error

type Errors = OpenUnion
  '[ NotFound AppDomain
   ]

class Monad m => Retriever m where
  allForApp :: AppId -> m [Entity AppDomain]
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
