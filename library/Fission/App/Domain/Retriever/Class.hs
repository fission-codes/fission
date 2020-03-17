module Fission.App.Domain.Retriever.Class
  ( Retriever (..)
  , Errors
  ) where

import           Database.Persist

import           Fission.Prelude
import           Fission.Error
import           Fission.Models
import           Fission.URL

type Errors = OpenUnion
  '[ NotFound AppDomain
   ]

class Monad m => Retriever m where
  allForApp           :: AppId -> m [Entity AppDomain]
  allSiblingsByDomain :: DomainName -> Maybe Subdomain -> m (Either Errors [Entity AppDomain])

instance MonadIO m => Retriever (Transaction m) where
  allForApp appId = selectList [AppDomainAppId ==. appId] []

  allSiblingsByDomain domainName maySubdomain = do
    mayAppDomain <- selectFirst
      [ AppDomainDomainName ==. domainName
      , AppDomainSubdomain  ==. maySubdomain
      ]
      []

    case mayAppDomain of
      Nothing ->
        return . openLeft <| NotFound @AppDomain

      Just (Entity _ AppDomain {appDomainAppId}) ->
        Right <$> selectList [AppDomainAppId ==. appDomainAppId] []
